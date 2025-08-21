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
var compiling_chunk: *Chunk = undefined;

pub fn compile(vm: *VM, source: []const u8, chunk: *Chunk) !bool {
    defer end_compiler() catch {};

    parser = .{
        .current = undefined,
        .previous = undefined,
        .had_error = false,
        .panic_mode = false,
        .vm = vm,
        .ctx = vm.ctx,
    };

    lexer = Lexer.init(source);
    defer lexer.deinit();

    compiling_chunk = chunk;

    try advance();

    while (!try match(TokenType.EOF)) {
        try declaration();
    }

    try consume(TokenType.EOF, "Expected end of expression.");

    return !parser.had_error;
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

fn expression() !void {
    try parse_precedence(Precedence.ASSIGNMENT);
}

fn expression_statement() !void {
    try expression();
    try consume(TokenType.SEMICOLON, "Expect ';' after expression.");
    try emit_byte(OpCode.POP);
}

fn declaration() !void {
    if (try match(TokenType.VAR)) {
        try var_declaration();
    } else {
        try statement();
    }

    if (parser.panic_mode) {
        try synchronize();
    }
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
    try emit_bytes(OpCode.DEFINE_GLOBAL, global);
}

fn parse_variable(comptime error_message: []const u8) !u8 {
    try consume(TokenType.IDENTIFIER, error_message);
    return try identifier_constant(&parser.previous);
}

fn identifier_constant(name: *const Token) !u8 {
    const str = try objects.copy_string(parser.vm, name.token);
    return make_constant(.{ .object = @ptrCast(str) });
}

fn statement() !void {
    if (try match(TokenType.PRINT)) {
        try print_statement();
    } else {
        try expression_statement();
    }
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
    const arg = try identifier_constant(name);

    if (can_assign and try match(TokenType.EQUAL)) {
        try expression();
        try emit_bytes(OpCode.SET_GLOBAL, arg);
    } else {
        try emit_bytes(OpCode.GET_GLOBAL, arg);
    }
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
    .{ TokenType.LEFT_PAREN, grouping, null, Precedence.NONE },
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
    .{ TokenType.AND, null, null, Precedence.NONE },
    .{ TokenType.CLASS, null, null, Precedence.NONE },
    .{ TokenType.ELSE, null, null, Precedence.NONE },
    .{ TokenType.FALSE, literal, null, Precedence.NONE },
    .{ TokenType.FOR, null, null, Precedence.NONE },
    .{ TokenType.FN, null, null, Precedence.NONE },
    .{ TokenType.IF, null, null, Precedence.NONE },
    .{ TokenType.NIL, literal, null, Precedence.NONE },
    .{ TokenType.OR, null, null, Precedence.NONE },
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

fn end_compiler() !void {
    try emit_return();

    if (comptime DEBUG_TRACING) {
        if (!parser.had_error) {
            try debug.disassemble_chunk(current_chunk(), "code", parser.ctx.stdout);
        }
    }
}

fn current_chunk() *Chunk {
    return compiling_chunk;
}

fn emit_byte(byte: anytype) !void {
    try current_chunk().write_byte(byte, parser.previous.line);
}

fn emit_bytes(byte1: anytype, byte2: anytype) !void {
    try emit_byte(byte1);
    try emit_byte(byte2);
}

fn emit_return() !void {
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
