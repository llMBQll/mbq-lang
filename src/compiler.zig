const std = @import("std");

const chunks = @import("chunks.zig");
const context = @import("context.zig");
const debug = @import("debug.zig");
const lexer_mod = @import("lexer.zig");
const objects = @import("objects.zig");
const values = @import("values.zig");
const vm_mod = @import("vm.zig");

const DEBUG_TRACING = true;

const Chunk = chunks.Chunk;
const Lexer = lexer_mod.Lexer;
const Function = objects.Function;
const OpCode = chunks.OpCode;
const Token = lexer_mod.Token;
const TokenType = lexer_mod.TokenType;
const Value = values.Value;
const VM = vm_mod.VM;

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
    vm: *VM,
    ctx: context.Context,
};

const Compiler = struct {
    const MAx_LOCAL_COUNT = std.math.maxInt(u8) + 1;

    enclosing: *Compiler,
    function: *Function,
    fn_type: FnType,

    locals: [MAx_LOCAL_COUNT]Local,
    local_count: u32,
    scope_depth: u32,
};

const FnType = enum {
    FUNCTION,
    SCRIPT,
};

const Local = struct {
    name: Token,
    depth: ?u32,
};

const Precedence = enum {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
};

const ParseFn = *const fn (bool) anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

var lexer: Lexer = undefined;
var parser: Parser = undefined;
var current: *Compiler = undefined;

pub fn compile(vm: *VM, source: []const u8) !?*Function {
    parser = .{
        .current = undefined,
        .previous = undefined,
        .had_error = false,
        .panic_mode = false,
        .vm = vm,
        .ctx = vm.ctx,
    };

    var compiler: Compiler = undefined;
    try init_compiler(&compiler, vm, FnType.SCRIPT);

    lexer = Lexer.init(source);
    defer lexer.deinit();

    try advance();

    while (!try match(TokenType.EOF)) {
        try declaration();
    }

    try consume(TokenType.EOF, "Expected end of expression.");

    const func = try end_compiler();
    return if (parser.had_error) null else func;
}

fn init_compiler(compiler: *Compiler, vm: *VM, fn_type: FnType) !void {
    compiler.enclosing = current;
    compiler.function = try objects.new_function(vm);
    compiler.fn_type = fn_type;
    compiler.local_count = 0;
    compiler.scope_depth = 0;

    const local = &compiler.locals[compiler.local_count];
    compiler.local_count += 1;
    local.depth = 0;
    local.name.token = "";

    if (fn_type != FnType.SCRIPT) {
        compiler.function.name = try objects.copy_string(vm, parser.previous.token);
    }

    current = compiler;
}

fn end_compiler() !*Function {
    try emit_return();
    const func = current.function;

    if (comptime DEBUG_TRACING) {
        if (!parser.had_error) {
            try debug.disassemble_chunk(
                current_chunk(),
                if (func.name) |name| name.chars else "<script>",
                parser.ctx.stdout,
            );
        }
    }

    current = current.enclosing;

    return func;
}

fn parse_precedence(precedence: Precedence) !void {
    try advance();

    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);

    const prefix_rule = get_rule(parser.previous.token_type).prefix orelse {
        try err("Expected expression.");
        return;
    };
    try prefix_rule(can_assign);

    while (@intFromEnum(precedence) <= @intFromEnum(get_rule(parser.current.token_type).precedence)) {
        try advance();
        const infix_rule = get_rule(parser.previous.token_type).infix;
        try infix_rule.?(can_assign);
    }

    if (can_assign and try match(TokenType.EQUAL)) {
        try err("Invalid assignment target.");
    }
}

fn literal(_: bool) !void {
    switch (parser.previous.token_type) {
        TokenType.FALSE => try emit_byte(OpCode.FALSE),
        TokenType.NIL => try emit_byte(OpCode.NIL),
        TokenType.TRUE => try emit_byte(OpCode.TRUE),
        else => return, // Unreachable.
    }
}

fn binary(_: bool) !void {
    const operator_type = parser.previous.token_type;
    const rule = get_rule(operator_type);
    try parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (operator_type) {
        TokenType.BANG_EQUAL => try emit_bytes(OpCode.EQUAL, OpCode.NOT),
        TokenType.EQUAL_EQUAL => try emit_byte(OpCode.EQUAL),
        TokenType.GREATER => try emit_byte(OpCode.GREATER),
        TokenType.GREATER_EQUAL => try emit_bytes(OpCode.LESS, OpCode.NOT),
        TokenType.LESS => try emit_byte(OpCode.LESS),
        TokenType.LESS_EQUAL => try emit_bytes(OpCode.GREATER, OpCode.NOT),
        TokenType.PLUS => try emit_byte(OpCode.ADD),
        TokenType.MINUS => try emit_byte(OpCode.SUBTRACT),
        TokenType.STAR => try emit_byte(OpCode.MULTIPLY),
        TokenType.SLASH => try emit_byte(OpCode.DIVIDE),
        else => return, // Unreachable.
    }
}

fn unary(_: bool) !void {
    const operator_type = parser.previous.token_type;

    try parse_precedence(Precedence.UNARY);

    switch (operator_type) {
        TokenType.BANG => try emit_byte(OpCode.NOT),
        TokenType.MINUS => try emit_byte(OpCode.NEGATE),
        else => return, // unreachable
    }
}

fn grouping(_: bool) !void {
    try expression();
    try consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn call(_: bool) !void {
    const arg_count = try argument_list();
    try emit_bytes(OpCode.CALL, arg_count);
}

fn argument_list() !u8 {
    var arg_count: u8 = 0;
    if (!check(TokenType.RIGHT_PAREN)) {
        while (true) {
            try expression();

            if (arg_count == 255) {
                try err("Can't have more than 255 arguments.");
            }
            arg_count += 1;

            if (!try match(TokenType.COMMA)) {
                break;
            }
        }
    }
    try consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

fn @"and"(_: bool) !void {
    const end_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    try emit_byte(OpCode.POP);
    try parse_precedence(Precedence.AND);
    try patch_jump(end_jump);
}

fn @"or"(_: bool) !void {
    const else_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    const end_jump = try emit_jump(OpCode.JUMP);

    try patch_jump(else_jump);
    try emit_byte(OpCode.POP);

    try parse_precedence(Precedence.OR);
    try patch_jump(end_jump);
}

fn expression() !void {
    try parse_precedence(Precedence.ASSIGNMENT);
}

fn expression_statement() !void {
    try expression();
    try consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    try emit_byte(OpCode.POP);
}

fn declaration() anyerror!void { // 'unable to resolve inferred error set' without `anyerror` - TODO look into that
    if (try match(TokenType.FN)) {
        try fn_declaration();
    } else if (try match(TokenType.VAR)) {
        try var_declaration();
    } else {
        try statement();
    }

    if (parser.panic_mode) {
        try synchronize();
    }
}

fn fn_declaration() !void {
    const global = try parse_variable("Expect function name.");
    mark_initialized();
    try function(FnType.FUNCTION);
    try define_variable(global);
}

fn function(fn_type: FnType) !void {
    var compiler: Compiler = undefined;
    try init_compiler(&compiler, parser.vm, fn_type);
    begin_scope();

    try consume(TokenType.LEFT_PAREN, "Expect '(' after function name.");

    if (!check(TokenType.RIGHT_PAREN)) {
        while (true) {
            current.function.arity += 1;
            if (current.function.arity > 255) {
                try error_at_current("Can't have more than 255 parameters");
            }
            const constant = try parse_variable("Expect parameter name.");
            try define_variable(constant);

            if (!try match(TokenType.COMMA)) {
                break;
            }
        }
    }

    try consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");
    try consume(TokenType.LEFT_BRACE, "Expect '{' before function body.");
    try block();

    // end_scope not required, as the compiler is ended in the next line anyways
    // try end_scope();

    const func = try end_compiler();
    const arg = try make_constant(.{ .object = @ptrCast(func) });
    try emit_bytes(OpCode.CONSTANT, arg);
}

fn var_declaration() !void {
    const global = try parse_variable("Expect variable name.");

    if (try match(TokenType.EQUAL)) {
        try expression();
    } else {
        try emit_byte(OpCode.NIL);
    }
    try consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");

    try define_variable(global);
}

fn define_variable(global: u8) !void {
    if (current.scope_depth > 0) {
        mark_initialized();
        return;
    }

    try emit_bytes(OpCode.DEFINE_GLOBAL, global);
}

fn mark_initialized() void {
    if (current.scope_depth == 0) {
        return;
    }
    current.locals[current.local_count - 1].depth = current.scope_depth;
}

fn declare_variable() !void {
    if (current.scope_depth == 0) {
        return;
    }

    const name = &parser.previous;

    var i = current.local_count;
    while (i > 0) {
        i -= 1;
        const local = &current.locals[i];
        if (local.depth != null and local.depth.? < current.scope_depth) {
            break;
        }

        if (std.mem.eql(u8, name.token, local.name.token)) {
            try err("Already a variable with this name in this scope.");
        }
    }

    try add_local(name);
}

fn add_local(name: *Token) !void {
    if (current.local_count == Compiler.MAx_LOCAL_COUNT) {
        try err("Too many local variables in function.");
        return;
    }

    const local = &current.locals[current.local_count];
    current.local_count += 1;

    local.name = name.*;
    local.depth = null;
}

fn parse_variable(comptime error_message: []const u8) !u8 {
    try consume(TokenType.IDENTIFIER, error_message);

    try declare_variable();
    if (current.scope_depth > 0) {
        return 0;
    }

    return try identifier_constant(&parser.previous);
}

fn identifier_constant(name: *const Token) !u8 {
    const str = try objects.copy_string(parser.vm, name.token);
    return make_constant(.{ .object = @ptrCast(str) });
}

fn statement() anyerror!void {
    if (try match(TokenType.PRINT)) {
        try print_statement();
    } else if (try match(TokenType.IF)) {
        try if_statement();
    } else if (try match(TokenType.RETURN)) {
        try return_statement();
    } else if (try match(TokenType.WHILE)) {
        try while_statement();
    } else if (try match(TokenType.FOR)) {
        try for_statement();
    } else if (try match(TokenType.LEFT_BRACE)) {
        begin_scope();
        try block();
        try end_scope();
    } else {
        try expression_statement();
    }
}

fn return_statement() !void {
    if (current.fn_type == FnType.SCRIPT) {
        try err("Can't return from top-level code.");
    }

    if (try match(TokenType.SEMICOLON)) {
        try emit_return();
    } else {
        try expression();
        try consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        try emit_byte(OpCode.RETURN);
    }
}

fn for_statement() !void {
    begin_scope();

    // Pre-loop
    try consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
    if (try match(TokenType.SEMICOLON)) {
        // No initializer.
    } else if (try match(TokenType.VAR)) {
        try var_declaration();
    } else {
        try expression_statement();
    }

    // Condition
    var loop_start = current_chunk().code.items.len;

    var exit_jump: ?usize = null;
    if (!try match(TokenType.SEMICOLON)) {
        try expression();
        try consume(TokenType.SEMICOLON, "Expect ';' after loop condition.");

        exit_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
        try emit_byte(OpCode.POP);
    }

    // Post-loop
    if (!try match(TokenType.RIGHT_PAREN)) {
        const body_jump = try emit_jump(OpCode.JUMP);
        const post_loop_start = current_chunk().code.items.len;
        try expression();
        try emit_byte(OpCode.POP);
        try consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.");

        try emit_loop(loop_start);
        loop_start = post_loop_start;
        try patch_jump(body_jump);
    }

    // Loop body
    try statement();
    try emit_loop(loop_start);

    if (exit_jump) |jump| {
        try patch_jump(jump);
        try emit_byte(OpCode.POP);
    }

    try end_scope();
}

fn while_statement() !void {
    const loop_start = current_chunk().code.items.len;
    try consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    try expression();
    try consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const exit_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    try emit_byte(OpCode.POP);
    try statement();
    try emit_loop(loop_start);

    try patch_jump(exit_jump);
    try emit_byte(OpCode.POP);
}

fn emit_loop(loop_start: usize) !void {
    try emit_byte(OpCode.LOOP);

    const offset = current_chunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        try err("Loop body too large.");
    }

    const high: u8 = @truncate(offset >> 8);
    const low: u8 = @truncate(offset);
    try emit_byte(high);
    try emit_byte(low);
}

fn if_statement() !void {
    try consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    try expression();
    try consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const then_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    try emit_byte(OpCode.POP);
    try statement();

    const else_jump = try emit_jump(OpCode.JUMP);

    try patch_jump(then_jump);
    try emit_byte(OpCode.POP);

    if (try match(TokenType.ELSE)) {
        try statement();
    }

    try patch_jump(else_jump);
}

fn emit_jump(instruction: OpCode) !usize {
    try emit_byte(instruction);
    try emit_byte(0xFF);
    try emit_byte(0xFF);
    return current_chunk().code.items.len - 2;
}

fn patch_jump(offset: usize) !void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump = current_chunk().code.items.len - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        try err("Too much code to jump over.");
    }

    current_chunk().code.items[offset] = @truncate(jump >> 8);
    current_chunk().code.items[offset + 1] = @truncate(jump);
}

fn begin_scope() void {
    current.scope_depth += 1;
}

fn end_scope() !void {
    current.scope_depth -= 1;

    while (current.local_count > 0) {
        const depth = current.locals[current.local_count - 1].depth;
        if (depth == null or depth.? <= current.scope_depth) {
            break;
        }

        try emit_byte(OpCode.POP);
        current.local_count -= 1;
    }
}

fn block() !void {
    while (!check(TokenType.RIGHT_BRACE) and !check(TokenType.EOF)) {
        try declaration();
    }

    try consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
}

fn print_statement() !void {
    try expression();
    try consume(TokenType.SEMICOLON, "Expect ';' after value.");
    try emit_byte(OpCode.PRINT);
}

fn synchronize() !void {
    parser.panic_mode = false;

    while (parser.current.token_type != TokenType.EOF) {
        if (parser.previous.token_type == TokenType.SEMICOLON) {
            return;
        }

        switch (parser.current.token_type) {
            TokenType.CLASS,
            TokenType.FN,
            TokenType.VAR,
            TokenType.FOR,
            TokenType.IF,
            TokenType.WHILE,
            TokenType.PRINT,
            TokenType.RETURN,
            => {
                return;
            },
            else => {
                // Do nothing
            },
        }

        try advance();
    }
}

fn number(_: bool) !void {
    const value = try std.fmt.parseFloat(f64, parser.previous.token);
    try emit_constant(.{ .number = value });
}

fn string(_: bool) !void {
    const len = parser.previous.token.len;
    const str = try objects.copy_string(parser.vm, parser.previous.token[1 .. len - 1]);
    const val = Value{ .object = @ptrCast(str) };
    try emit_constant(val);
}

fn variable(can_assign: bool) !void {
    try named_variable(&parser.previous, can_assign);
}

fn named_variable(name: *const Token, can_assign: bool) !void {
    const maybe_local = try resolve_local(current, name);

    const arg, const get, const set = if (maybe_local) |local| blk: {
        break :blk .{ local, OpCode.GET_LOCAL, OpCode.SET_LOCAL };
    } else blk: {
        const global = try identifier_constant(name);
        break :blk .{ global, OpCode.GET_GLOBAL, OpCode.SET_GLOBAL };
    };

    if (can_assign and try match(TokenType.EQUAL)) {
        try expression();
        try emit_bytes(set, arg);
    } else {
        try emit_bytes(get, arg);
    }
}

fn resolve_local(compiler: *Compiler, name: *const Token) !?u8 {
    var i = compiler.local_count;
    while (i > 0) {
        i -= 1;
        const local = &compiler.locals[i];
        if (std.mem.eql(u8, name.token, local.name.token)) {
            if (local.depth == null) {
                try err("Can't read local variable in its own initializer.");
            }
            return @truncate(i);
        }
    }
    return null;
}

fn get_rule(token_type: TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

const rule_count = @typeInfo(TokenType).@"enum".fields.len;

fn make_parse_rule_table(input_table: [rule_count]struct { TokenType, ?ParseFn, ?ParseFn, Precedence }) [rule_count]ParseRule {
    var output: [rule_count]ParseRule = undefined;

    for (0..rule_count) |idx| {
        const token_type = input_table[idx].@"0";

        if (@intFromEnum(token_type) != idx) {
            @compileError(std.fmt.comptimePrint(
                "Parse rule out of order. {s} ({d}), found at index {d}",
                .{ @tagName(token_type), @intFromEnum(token_type), idx },
            ));
        }
        output[idx] = .{
            .prefix = input_table[idx].@"1",
            .infix = input_table[idx].@"2",
            .precedence = input_table[idx].@"3",
        };
    }

    return output;
}

const rules = make_parse_rule_table(.{
    .{ TokenType.LEFT_PAREN, grouping, call, Precedence.CALL },
    .{ TokenType.RIGHT_PAREN, null, null, Precedence.NONE },
    .{ TokenType.LEFT_BRACE, null, null, Precedence.NONE },
    .{ TokenType.RIGHT_BRACE, null, null, Precedence.NONE },
    .{ TokenType.COMMA, null, null, Precedence.NONE },
    .{ TokenType.DOT, null, null, Precedence.NONE },
    .{ TokenType.MINUS, unary, binary, Precedence.TERM },
    .{ TokenType.PLUS, null, binary, Precedence.TERM },
    .{ TokenType.SEMICOLON, null, null, Precedence.NONE },
    .{ TokenType.SLASH, null, binary, Precedence.FACTOR },
    .{ TokenType.STAR, null, binary, Precedence.FACTOR },
    .{ TokenType.BANG, unary, null, Precedence.NONE },
    .{ TokenType.BANG_EQUAL, null, binary, Precedence.EQUALITY },
    .{ TokenType.EQUAL, null, null, Precedence.NONE },
    .{ TokenType.EQUAL_EQUAL, null, binary, Precedence.EQUALITY },
    .{ TokenType.GREATER, null, binary, Precedence.COMPARISON },
    .{ TokenType.GREATER_EQUAL, null, binary, Precedence.COMPARISON },
    .{ TokenType.LESS, null, binary, Precedence.COMPARISON },
    .{ TokenType.LESS_EQUAL, null, binary, Precedence.COMPARISON },
    .{ TokenType.IDENTIFIER, variable, null, Precedence.NONE },
    .{ TokenType.STRING, string, null, Precedence.NONE },
    .{ TokenType.NUMBER, number, null, Precedence.NONE },
    .{ TokenType.AND, null, @"and", Precedence.AND },
    .{ TokenType.CLASS, null, null, Precedence.NONE },
    .{ TokenType.ELSE, null, null, Precedence.NONE },
    .{ TokenType.FALSE, literal, null, Precedence.NONE },
    .{ TokenType.FOR, null, null, Precedence.NONE },
    .{ TokenType.FN, null, null, Precedence.NONE },
    .{ TokenType.IF, null, null, Precedence.NONE },
    .{ TokenType.NIL, literal, null, Precedence.NONE },
    .{ TokenType.OR, null, @"or", Precedence.OR },
    .{ TokenType.PRINT, null, null, Precedence.NONE },
    .{ TokenType.RETURN, null, null, Precedence.NONE },
    .{ TokenType.SUPER, null, null, Precedence.NONE },
    .{ TokenType.THIS, null, null, Precedence.NONE },
    .{ TokenType.TRUE, literal, null, Precedence.NONE },
    .{ TokenType.VAR, null, null, Precedence.NONE },
    .{ TokenType.WHILE, null, null, Precedence.NONE },
    .{ TokenType.ERROR, null, null, Precedence.NONE },
    .{ TokenType.EOF, null, null, Precedence.NONE },
});

fn current_chunk() *Chunk {
    return &current.function.chunk;
}

fn emit_byte(byte: anytype) !void {
    try current_chunk().write_byte(byte, parser.previous.line);
}

fn emit_bytes(byte1: anytype, byte2: anytype) !void {
    try emit_byte(byte1);
    try emit_byte(byte2);
}

fn emit_return() !void {
    try emit_byte(OpCode.NIL);
    try emit_byte(OpCode.RETURN);
}

fn emit_constant(value: Value) !void {
    try emit_bytes(OpCode.CONSTANT, try make_constant(value));
}

fn make_constant(value: Value) !u8 {
    const constant = try current_chunk().add_constant(value);

    if (constant > std.math.maxInt(u8)) {
        try err("Too many constants in one chunk.");
        return 0;
    }

    return @truncate(constant);
}

fn advance() !void {
    parser.previous = parser.current;

    while (true) {
        parser.current = lexer.next();

        if (parser.current.token_type != TokenType.ERROR) {
            break;
        }

        try error_at_current(parser.current.token);
    }
}

fn consume(token_type: TokenType, message: []const u8) !void {
    if (parser.current.token_type == token_type) {
        try advance();
    } else {
        try error_at_current(message);
    }
}

fn check(token_type: TokenType) bool {
    return parser.current.token_type == token_type;
}

fn match(token_type: TokenType) !bool {
    if (!check(token_type)) {
        return false;
    }
    try advance();
    return true;
}

fn error_at_current(message: []const u8) !void {
    return error_at(parser.current, message);
}

fn err(message: []const u8) !void {
    return error_at(parser.previous, message);
}

fn error_at(token: Token, message: []const u8) !void {
    if (parser.panic_mode) {
        return;
    }
    parser.panic_mode = true;

    const stderr = parser.ctx.stderr;

    try stderr.print("[line {d}] Error", .{token.line});

    if (token.token_type == TokenType.EOF) {
        try stderr.print(" at end", .{});
    } else if (token.token_type == TokenType.ERROR) {
        // Do nothing
    } else {
        try stderr.print(" at '{s}'", .{token.token});
    }

    try stderr.print(": {s}\n", .{message});
    parser.had_error = true;
}
