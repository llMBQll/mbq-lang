const std = @import("std");

const chunks = @import("chunks.zig");
const config = @import("config.zig");
const debug = @import("debug.zig");
const lexer_mod = @import("lexer.zig");
const objects = @import("objects.zig");
const values = @import("values.zig");
const vm_mod = @import("vm.zig");

const Allocator = std.mem.Allocator;
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
};

const Compiler = struct {
    const MAx_LOCAL_COUNT = std.math.maxInt(u8) + 1;
    const MAX_UPVALUE_COUNT = std.math.maxInt(u8) + 1;

    enclosing: ?*Compiler,
    function: *Function,
    fn_type: FnType,

    locals: [MAx_LOCAL_COUNT]Local,
    local_count: u32,
    upvalues: [MAX_UPVALUE_COUNT]Upvalue,
    scope_depth: u32,
};

const FnType = enum {
    FUNCTION,
    SCRIPT,
};

const Local = struct {
    name: Token,
    depth: ?u32,
    is_captured: bool,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
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

const ParseFn = *const fn (bool) Allocator.Error!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

var lexer: Lexer = undefined;
var parser: Parser = undefined;
var current: *Compiler = undefined;

pub fn compile(vm: *VM, source: []const u8) Allocator.Error!?*Function {
    parser = .{
        .current = undefined,
        .previous = undefined,
        .had_error = false,
        .panic_mode = false,
        .vm = vm,
    };

    var compiler: Compiler = undefined;
    try init_compiler(&compiler, vm, FnType.SCRIPT, null);

    lexer = Lexer.init(source);
    defer lexer.deinit();

    advance();

    while (!match(TokenType.EOF)) {
        try declaration();
    }

    consume(TokenType.EOF, "Expected end of expression.");

    const func = try end_compiler();
    return if (parser.had_error) null else func;
}

fn init_compiler(compiler: *Compiler, vm: *VM, fn_type: FnType, previous: ?*Compiler) Allocator.Error!void {
    compiler.enclosing = previous;
    compiler.function = try objects.new_function(vm);
    compiler.fn_type = fn_type;
    compiler.local_count = 0;
    compiler.scope_depth = 0;

    const local = &compiler.locals[compiler.local_count];
    compiler.local_count += 1;
    local.depth = 0;
    local.is_captured = false;
    local.name.token = "";

    if (fn_type != FnType.SCRIPT) {
        compiler.function.name = try objects.copy_string(vm, parser.previous.token);
    }

    current = compiler;
}

fn end_compiler() Allocator.Error!*Function {
    try emit_return();
    const func = current.function;

    if (comptime config.COMPTIME_DEBUG_TRACING) {
        if (!parser.had_error) {
            debug.disassemble_chunk(
                current_chunk(),
                if (func.name) |name| name.chars else "<script>",
                parser.vm.stdout,
            ) catch |e| {
                parser.vm.stderr.print("Failed to print deubg info {}", .{e}) catch {};
                parser.vm.stderr.flush() catch {};
            };
        }
    }

    current = if (current.enclosing) |enclosing| enclosing else undefined;

    return func;
}

fn parse_precedence(precedence: Precedence) Allocator.Error!void {
    advance();

    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);

    const prefix_rule = get_rule(parser.previous.token_type).prefix orelse {
        err("Expected expression.");
        return;
    };
    try prefix_rule(can_assign);

    while (@intFromEnum(precedence) <= @intFromEnum(get_rule(parser.current.token_type).precedence)) {
        advance();
        const infix_rule = get_rule(parser.previous.token_type).infix;
        try infix_rule.?(can_assign);
    }

    if (can_assign and match(TokenType.EQUAL)) {
        err("Invalid assignment target.");
    }
}

fn literal(_: bool) Allocator.Error!void {
    switch (parser.previous.token_type) {
        TokenType.FALSE => try emit_byte(OpCode.FALSE),
        TokenType.NIL => try emit_byte(OpCode.NIL),
        TokenType.TRUE => try emit_byte(OpCode.TRUE),
        else => return, // Unreachable.
    }
}

fn binary(_: bool) Allocator.Error!void {
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

fn unary(_: bool) Allocator.Error!void {
    const operator_type = parser.previous.token_type;

    try parse_precedence(Precedence.UNARY);

    switch (operator_type) {
        TokenType.BANG => try emit_byte(OpCode.NOT),
        TokenType.MINUS => try emit_byte(OpCode.NEGATE),
        else => return, // unreachable
    }
}

fn grouping(_: bool) Allocator.Error!void {
    try expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn call(_: bool) Allocator.Error!void {
    const arg_count = try argument_list();
    try emit_bytes(OpCode.CALL, arg_count);
}

fn argument_list() Allocator.Error!u8 {
    var arg_count: u8 = 0;
    if (!check(TokenType.RIGHT_PAREN)) {
        while (true) {
            try expression();

            if (arg_count == 255) {
                err("Can't have more than 255 arguments.");
            }
            arg_count += 1;

            if (!match(TokenType.COMMA)) {
                break;
            }
        }
    }
    consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments.");
    return arg_count;
}

fn @"and"(_: bool) Allocator.Error!void {
    const end_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    try emit_byte(OpCode.POP);
    try parse_precedence(Precedence.AND);
    patch_jump(end_jump);
}

fn @"or"(_: bool) Allocator.Error!void {
    const else_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    const end_jump = try emit_jump(OpCode.JUMP);

    patch_jump(else_jump);
    try emit_byte(OpCode.POP);

    try parse_precedence(Precedence.OR);
    patch_jump(end_jump);
}

fn expression() Allocator.Error!void {
    try parse_precedence(Precedence.ASSIGNMENT);
}

fn expression_statement() Allocator.Error!void {
    try expression();
    consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    try emit_byte(OpCode.POP);
}

fn declaration() Allocator.Error!void {
    if (match(TokenType.FN)) {
        try fn_declaration();
    } else if (match(TokenType.VAR)) {
        try var_declaration();
    } else {
        try statement();
    }

    if (parser.panic_mode) {
        synchronize();
    }
}

fn fn_declaration() Allocator.Error!void {
    const global = try parse_variable("Expect function name.");
    mark_initialized();
    try function(FnType.FUNCTION);
    try define_variable(global);
}

fn function(fn_type: FnType) Allocator.Error!void {
    var compiler: Compiler = undefined;
    try init_compiler(&compiler, parser.vm, fn_type, current);
    begin_scope();

    consume(TokenType.LEFT_PAREN, "Expect '(' after function name.");

    if (!check(TokenType.RIGHT_PAREN)) {
        while (true) {
            current.function.arity += 1;
            if (current.function.arity > 255) {
                error_at_current("Can't have more than 255 parameters");
            }
            const constant = try parse_variable("Expect parameter name.");
            try define_variable(constant);

            if (!match(TokenType.COMMA)) {
                break;
            }
        }
    }

    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters.");
    consume(TokenType.LEFT_BRACE, "Expect '{' before function body.");
    try block();

    // end_scope not required, as the compiler is ended in the next line anyways
    // try end_scope();

    const func = try end_compiler();
    const arg = try make_constant(.{ .object = @ptrCast(func) });
    try emit_bytes(OpCode.CLOSURE, arg);

    for (0..func.upvalue_count) |i| {
        try emit_byte(@intFromBool(compiler.upvalues[i].is_local));
        try emit_byte(compiler.upvalues[i].index);
    }
}

fn var_declaration() Allocator.Error!void {
    const global = try parse_variable("Expect variable name.");

    if (match(TokenType.EQUAL)) {
        try expression();
    } else {
        try emit_byte(OpCode.NIL);
    }
    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration.");

    try define_variable(global);
}

fn define_variable(global: u8) Allocator.Error!void {
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

fn declare_variable() void {
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
            err("Already a variable with this name in this scope.");
        }
    }

    add_local(name);
}

fn add_local(name: *Token) void {
    if (current.local_count == Compiler.MAx_LOCAL_COUNT) {
        err("Too many local variables in function.");
        return;
    }

    const local = &current.locals[current.local_count];
    current.local_count += 1;

    local.name = name.*;
    local.depth = null;
    local.is_captured = false;
}

fn parse_variable(comptime error_message: []const u8) Allocator.Error!u8 {
    consume(TokenType.IDENTIFIER, error_message);

    declare_variable();
    if (current.scope_depth > 0) {
        return 0;
    }

    return identifier_constant(&parser.previous);
}

fn identifier_constant(name: *const Token) Allocator.Error!u8 {
    const str = try objects.copy_string(parser.vm, name.token);
    return make_constant(.{ .object = @ptrCast(str) });
}

fn statement() Allocator.Error!void {
    if (match(TokenType.PRINT)) {
        try print_statement();
    } else if (match(TokenType.IF)) {
        try if_statement();
    } else if (match(TokenType.RETURN)) {
        try return_statement();
    } else if (match(TokenType.WHILE)) {
        try while_statement();
    } else if (match(TokenType.FOR)) {
        try for_statement();
    } else if (match(TokenType.LEFT_BRACE)) {
        begin_scope();
        try block();
        try end_scope();
    } else {
        try expression_statement();
    }
}

fn return_statement() Allocator.Error!void {
    if (current.fn_type == FnType.SCRIPT) {
        err("Can't return from top-level code.");
    }

    if (match(TokenType.SEMICOLON)) {
        try emit_return();
    } else {
        try expression();
        consume(TokenType.SEMICOLON, "Expect ';' after return value.");
        try emit_byte(OpCode.RETURN);
    }
}

fn for_statement() Allocator.Error!void {
    begin_scope();

    // Pre-loop
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.");
    if (match(TokenType.SEMICOLON)) {
        // No initializer.
    } else if (match(TokenType.VAR)) {
        try var_declaration();
    } else {
        try expression_statement();
    }

    // Condition
    var loop_start = current_chunk().code.items.len;

    var exit_jump: ?usize = null;
    if (!match(TokenType.SEMICOLON)) {
        try expression();
        consume(TokenType.SEMICOLON, "Expect ';' after loop condition.");

        exit_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
        try emit_byte(OpCode.POP);
    }

    // Post-loop
    if (!match(TokenType.RIGHT_PAREN)) {
        const body_jump = try emit_jump(OpCode.JUMP);
        const post_loop_start = current_chunk().code.items.len;
        try expression();
        try emit_byte(OpCode.POP);
        consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.");

        try emit_loop(loop_start);
        loop_start = post_loop_start;
        patch_jump(body_jump);
    }

    // Loop body
    try statement();
    try emit_loop(loop_start);

    if (exit_jump) |jump| {
        patch_jump(jump);
        try emit_byte(OpCode.POP);
    }

    try end_scope();
}

fn while_statement() Allocator.Error!void {
    const loop_start = current_chunk().code.items.len;
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'.");
    try expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const exit_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    try emit_byte(OpCode.POP);
    try statement();
    try emit_loop(loop_start);

    patch_jump(exit_jump);
    try emit_byte(OpCode.POP);
}

fn emit_loop(loop_start: usize) Allocator.Error!void {
    try emit_byte(OpCode.LOOP);

    const offset = current_chunk().code.items.len - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        err("Loop body too large.");
    }

    const high: u8 = @truncate(offset >> 8);
    const low: u8 = @truncate(offset);
    try emit_byte(high);
    try emit_byte(low);
}

fn if_statement() Allocator.Error!void {
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'.");
    try expression();
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition.");

    const then_jump = try emit_jump(OpCode.JUMP_IF_FALSE);
    try emit_byte(OpCode.POP);
    try statement();

    const else_jump = try emit_jump(OpCode.JUMP);

    patch_jump(then_jump);
    try emit_byte(OpCode.POP);

    if (match(TokenType.ELSE)) {
        try statement();
    }

    patch_jump(else_jump);
}

fn emit_jump(instruction: OpCode) Allocator.Error!usize {
    try emit_byte(instruction);
    try emit_byte(0xFF);
    try emit_byte(0xFF);
    return current_chunk().code.items.len - 2;
}

fn patch_jump(offset: usize) void {
    // -2 to adjust for the bytecode for the jump offset itself.
    const jump = current_chunk().code.items.len - offset - 2;

    if (jump > std.math.maxInt(u16)) {
        err("Too much code to jump over.");
    }

    current_chunk().code.items[offset] = @truncate(jump >> 8);
    current_chunk().code.items[offset + 1] = @truncate(jump);
}

fn begin_scope() void {
    current.scope_depth += 1;
}

fn end_scope() Allocator.Error!void {
    current.scope_depth -= 1;

    while (current.local_count > 0) {
        const depth = current.locals[current.local_count - 1].depth;
        if (depth == null or depth.? <= current.scope_depth) {
            break;
        }

        if (current.locals[current.local_count].is_captured) {
            try emit_byte(OpCode.CLOSE_UPVALUE);
        } else {
            try emit_byte(OpCode.POP);
        }

        current.local_count -= 1;
    }
}

fn block() Allocator.Error!void {
    while (!check(TokenType.RIGHT_BRACE) and !check(TokenType.EOF)) {
        try declaration();
    }

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block.");
}

fn print_statement() Allocator.Error!void {
    try expression();
    consume(TokenType.SEMICOLON, "Expect ';' after value.");
    try emit_byte(OpCode.PRINT);
}

fn synchronize() void {
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

        advance();
    }
}

fn number(_: bool) Allocator.Error!void {
    const value = std.fmt.parseFloat(f64, parser.previous.token) catch {
        return err("Failed to parse float. There is a bug in lexer implementation");
    };
    try emit_constant(.{ .number = value });
}

fn string(_: bool) Allocator.Error!void {
    const len = parser.previous.token.len;
    const str = try objects.copy_string(parser.vm, parser.previous.token[1 .. len - 1]);
    const val = Value{ .object = @ptrCast(str) };
    try emit_constant(val);
}

fn variable(can_assign: bool) !void {
    try named_variable(&parser.previous, can_assign);
}

fn named_variable(name: *const Token, can_assign: bool) Allocator.Error!void {
    const arg, const get, const set = blk: {
        if (resolve_local(current, name)) |local| {
            break :blk .{ local, OpCode.GET_LOCAL, OpCode.SET_LOCAL };
        } else if (resolve_upvalue(current, name)) |upvalue| {
            break :blk .{ upvalue, OpCode.GET_UPVALUE, OpCode.SET_UPVALUE };
        } else {
            const global = try identifier_constant(name);
            break :blk .{ global, OpCode.GET_GLOBAL, OpCode.SET_GLOBAL };
        }
    };

    if (can_assign and match(TokenType.EQUAL)) {
        try expression();
        try emit_bytes(set, arg);
    } else {
        try emit_bytes(get, arg);
    }
}

fn resolve_local(compiler: *Compiler, name: *const Token) ?u8 {
    var i = compiler.local_count;
    while (i > 0) {
        i -= 1;
        const local = &compiler.locals[i];
        if (std.mem.eql(u8, name.token, local.name.token)) {
            if (local.depth == null) {
                err("Can't read local variable in its own initializer.");
            }
            return @truncate(i);
        }
    }
    return null;
}

fn resolve_upvalue(compiler: *Compiler, name: *const Token) ?u8 {
    if (compiler.enclosing) |enclosing| {
        if (resolve_local(enclosing, name)) |local| {
            enclosing.locals[local].is_captured = true;
            return add_upvalue(compiler, local, true);
        } else if (resolve_upvalue(enclosing, name)) |upvalue| {
            return add_upvalue(compiler, upvalue, false);
        }
    }
    return null;
}

fn add_upvalue(compiler: *Compiler, index: u8, is_local: bool) ?u8 {
    const count = compiler.function.upvalue_count;

    for (0..count) |i| {
        const upvalue = &compiler.upvalues[i];
        if (upvalue.index == index and upvalue.is_local == is_local) {
            return @truncate(i);
        }
    }

    if (count == Compiler.MAX_UPVALUE_COUNT) {
        err("Too many closure variables in function.");
        return 0;
    }

    compiler.upvalues[count].is_local = is_local;
    compiler.upvalues[count].index = index;

    compiler.function.upvalue_count += 1;
    return @truncate(count);
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

fn emit_byte(byte: anytype) Allocator.Error!void {
    try current_chunk().write_byte(&parser.vm.memory, byte, parser.previous.line);
}

fn emit_bytes(byte1: anytype, byte2: anytype) Allocator.Error!void {
    try emit_byte(byte1);
    try emit_byte(byte2);
}

fn emit_return() Allocator.Error!void {
    try emit_byte(OpCode.NIL);
    try emit_byte(OpCode.RETURN);
}

fn emit_constant(value: Value) Allocator.Error!void {
    try emit_bytes(OpCode.CONSTANT, try make_constant(value));
}

fn make_constant(value: Value) Allocator.Error!u8 {
    const constant = try current_chunk().add_constant(&parser.vm.memory, value);

    if (constant > std.math.maxInt(u8)) {
        err("Too many constants in one chunk.");
        return 0;
    }

    return @truncate(constant);
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = lexer.next();

        if (parser.current.token_type != TokenType.ERROR) {
            break;
        }

        error_at_current(parser.current.token);
    }
}

fn consume(token_type: TokenType, message: []const u8) void {
    if (parser.current.token_type == token_type) {
        advance();
    } else {
        error_at_current(message);
    }
}

fn check(token_type: TokenType) bool {
    return parser.current.token_type == token_type;
}

fn match(token_type: TokenType) bool {
    if (!check(token_type)) {
        return false;
    }
    advance();
    return true;
}

fn error_at_current(message: []const u8) void {
    return error_at(parser.current, message);
}

fn err(message: []const u8) void {
    return error_at(parser.previous, message);
}

fn error_at(token: Token, message: []const u8) void {
    if (parser.panic_mode) {
        return;
    }
    parser.panic_mode = true;

    const stderr = parser.vm.stderr;

    stderr.print("[line {d}] Error", .{token.line}) catch {};

    if (token.token_type == TokenType.EOF) {
        stderr.print(" at end", .{}) catch {};
    } else if (token.token_type == TokenType.ERROR) {
        // Do nothing
    } else {
        stderr.print(" at '{s}'", .{token.token}) catch {};
    }

    stderr.print(": {s}\n", .{message}) catch {};
    stderr.flush() catch {};
    parser.had_error = true;
}
