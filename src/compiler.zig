const std = @import("std");

const debug = @import("debug.zig");
const chunks = @import("chunks.zig");
const lexer_mod = @import("lexer.zig");
const values = @import("values.zig");

const DEBUG_TRACING = true;

const Chunk = chunks.Chunk;
const Lexer = lexer_mod.Lexer;
const OpCode = chunks.OpCode;
const Token = lexer_mod.Token;
const TokenType = lexer_mod.TokenType;
const Value = values.Value;

const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
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

const ParseFn = *const fn () anyerror!void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

var lexer: Lexer = undefined;
var parser: Parser = undefined;
var compiling_chunk: *Chunk = undefined;

pub fn compile(source: []const u8, chunk: *Chunk) !bool {
    defer end_compiler() catch {};

    parser = .{
        .current = undefined,
        .previous = undefined,
        .had_error = false,
        .panic_mode = false,
    };

    lexer = Lexer.init(source);
    defer lexer.deinit();

    compiling_chunk = chunk;

    try advance();
    try expression();
    try consume(TokenType.EOF, "Expected end of expression.");

    return !parser.had_error;
}

fn parse_precedence(precedence: Precedence) !void {
    try advance();

    const prefix_rule = get_rule(parser.previous.token_type).prefix;
    if (prefix_rule) |rule| {
        try rule();
    } else {
        try err("Expected expression.");
    }

    while (@intFromEnum(precedence) <= @intFromEnum(get_rule(parser.current.token_type).precedence)) {
        try advance();
        const infix_rule = get_rule(parser.previous.token_type).infix;
        try infix_rule.?();
    }
}

fn literal() !void {
    switch (parser.previous.token_type) {
        TokenType.FALSE => try emit_byte(OpCode.FALSE),
        TokenType.NIL => try emit_byte(OpCode.NIL),
        TokenType.TRUE => try emit_byte(OpCode.TRUE),
        else => return, // Unreachable.
    }
}

fn binary() !void {
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

fn unary() !void {
    const operator_type = parser.previous.token_type;

    try parse_precedence(Precedence.UNARY);

    switch (operator_type) {
        TokenType.BANG => try emit_byte(OpCode.NOT),
        TokenType.MINUS => try emit_byte(OpCode.NEGATE),
        else => return, // unreachable
    }
}

fn grouping() !void {
    try expression();
    try consume(TokenType.RIGHT_PAREN, "Expect ')' after expression.");
}

fn expression() !void {
    try parse_precedence(Precedence.ASSIGNMENT);
}

fn get_rule(token_type: TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

const rules = [_]ParseRule{
    .{ .prefix = grouping, .infix = null, .precedence = Precedence.NONE }, // [TokenType.LEFT_PAREN]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.RIGHT_PAREN]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.LEFT_BRACE]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.RIGHT_BRACE]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.COMMA]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.DOT]
    .{ .prefix = unary, .infix = binary, .precedence = Precedence.TERM }, // [TokenType.MINUS]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.TERM }, // [TokenType.PLUS]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.SEMICOLON]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.FACTOR }, // [TokenType.SLASH]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.FACTOR }, // [TokenType.STAR]
    .{ .prefix = unary, .infix = null, .precedence = Precedence.NONE }, // [TokenType.BANG]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.EQUALITY }, // [TokenType.BANG_EQUAL]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.EQUAL]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.EQUALITY }, // [TokenType.EQUAL_EQUAL]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.COMPARISON }, // [TokenType.GREATER]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.COMPARISON }, // [TokenType.GREATER_EQUAL]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.COMPARISON }, // [TokenType.LESS]
    .{ .prefix = null, .infix = binary, .precedence = Precedence.COMPARISON }, // [TokenType.LESS_EQUAL]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.IDENTIFIER]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.STRING]
    .{ .prefix = number, .infix = null, .precedence = Precedence.NONE }, // [TokenType.NUMBER]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.AND]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.CLASS]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.ELSE]
    .{ .prefix = literal, .infix = null, .precedence = Precedence.NONE }, // [TokenType.FALSE]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.FOR]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.FUN]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.IF]
    .{ .prefix = literal, .infix = null, .precedence = Precedence.NONE }, // [TokenType.NIL]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.OR]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.PRINT]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.RETURN]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.SUPER]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.THIS]
    .{ .prefix = literal, .infix = null, .precedence = Precedence.NONE }, // [TokenType.TRUE]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.VAR]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.WHILE]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.ERROR]
    .{ .prefix = null, .infix = null, .precedence = Precedence.NONE }, // [TokenType.EOF]
};

fn number() !void {
    const value = try std.fmt.parseFloat(f64, parser.previous.token);
    try emit_constant(.{ .number = value });
}

fn end_compiler() !void {
    try emit_return();

    if (comptime DEBUG_TRACING) {
        if (!parser.had_error) {
            try debug.disassemble_chunk(current_chunk(), "code");
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

    const stderr = std.io.getStdErr().writer();

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
