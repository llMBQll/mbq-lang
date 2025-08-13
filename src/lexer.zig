const std = @import("std");

pub const Lexer = struct {
    pub const Self = @This();

    source: []const u8,
    start: usize,
    current: usize,
    line: usize,
    column: usize,

    pub fn init(source: []const u8) Self {
        return .{
            .source = source,
            .start = 0,
            .current = 0,
            .line = 1,
            .column = 1,
        };
    }

    pub fn deinit(self: Self) void {
        _ = self;
    }

    pub fn next(self: *Self) Token {
        self.skip_whitespace();

        self.start = self.current;

        if (self.is_at_end()) {
            return self.make_token(TokenType.EOF);
        }

        const c = self.advance();
        switch (c) {
            '(' => return self.make_token(TokenType.LEFT_PAREN),
            ')' => return self.make_token(TokenType.RIGHT_PAREN),
            '{' => return self.make_token(TokenType.LEFT_BRACE),
            '}' => return self.make_token(TokenType.RIGHT_BRACE),
            ';' => return self.make_token(TokenType.SEMICOLON),
            ',' => return self.make_token(TokenType.COMMA),
            '.' => return self.make_token(TokenType.DOT),
            '-' => return self.make_token(TokenType.MINUS),
            '+' => return self.make_token(TokenType.PLUS),
            '/' => return self.make_token(TokenType.SLASH),
            '*' => return self.make_token(TokenType.STAR),
            '!' => return self.make_token(if (self.match('=')) TokenType.BANG_EQUAL else TokenType.BANG),
            '=' => return self.make_token(if (self.match('=')) TokenType.EQUAL_EQUAL else TokenType.EQUAL),
            '<' => return self.make_token(if (self.match('=')) TokenType.LESS_EQUAL else TokenType.LESS),
            '>' => return self.make_token(if (self.match('=')) TokenType.GREATER_EQUAL else TokenType.GREATER),
            '"' => return self.make_string(),
            '0'...'9' => return self.make_number(),
            'a'...'z', 'A'...'Z', '_' => return self.make_identifier(),
            else => return self.make_error_token("Unexpected token"),
        }
    }

    fn advance(self: *Self) u8 {
        const c = self.source[self.current];
        self.current += 1;
        self.column += 1;
        return c;
    }

    fn peek(self: *const Self) u8 {
        return if (self.current < self.source.len) self.source[self.current] else 0;
    }

    fn peek_next(self: *const Self) u8 {
        if (self.is_at_end()) {
            return 0;
        }
        return self.source[self.current + 1];
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.is_at_end()) {
            return false;
        }
        if (self.source[self.current] != expected) {
            return false;
        }
        _ = self.advance();
        return true;
    }

    fn skip_whitespace(self: *Self) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => _ = self.advance(),
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peek_next() == '/') {
                        while (self.peek() != '\n' and !self.is_at_end()) {
                            _ = self.advance();
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn is_at_end(self: *const Self) bool {
        return self.current == self.source.len;
    }

    fn make_string(self: *Self) Token {
        while (self.peek() != '"' and !self.is_at_end()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.is_at_end()) {
            return self.make_error_token("Unterminated string.");
        }

        _ = self.advance();
        return self.make_token(TokenType.STRING);
    }

    fn is_digit(c: u8) bool {
        return c >= '0' and c <= '9';
    }

    fn make_number(self: *Self) Token {
        while (is_digit(self.peek())) {
            _ = self.advance();
        }

        // Look for a fractional part.
        if (self.peek() == '.' and is_digit(self.peek_next())) {
            // Consume the ".".
            _ = self.advance();

            while (is_digit(self.peek())) {
                _ = self.advance();
            }
        }

        return self.make_token(TokenType.NUMBER);
    }

    fn is_alpha(c: u8) bool {
        return c >= 'a' and c <= 'z' or c >= 'A' and c <= 'Z' or c == '_';
    }

    fn make_identifier(self: *Self) Token {
        while (is_alpha(self.peek()) or is_digit(self.peek())) {
            _ = self.advance();
        }
        return self.make_token(identifier_type(self.make_token_text()));
    }

    fn identifier_type(token: []const u8) TokenType {
        const keywords = std.StaticStringMap(TokenType).initComptime(.{
            .{ "and", TokenType.AND },
            .{ "class", TokenType.CLASS },
            .{ "else", TokenType.ELSE },
            .{ "false", TokenType.FALSE },
            .{ "for", TokenType.FOR },
            .{ "fn", TokenType.FN },
            .{ "if", TokenType.IF },
            .{ "nil", TokenType.NIL },
            .{ "or", TokenType.OR },
            .{ "print", TokenType.PRINT },
            .{ "return", TokenType.RETURN },
            .{ "super", TokenType.SUPER },
            .{ "this", TokenType.THIS },
            .{ "true", TokenType.TRUE },
            .{ "var", TokenType.VAR },
            .{ "while", TokenType.WHILE },
        });

        return keywords.get(token) orelse TokenType.IDENTIFIER;
    }

    fn make_token_text(self: *const Self) []const u8 {
        return self.source[self.start..self.current];
    }

    fn make_token(self: *const Self, token_type: TokenType) Token {
        return .{
            .token_type = token_type,
            .token = self.make_token_text(),
            .line = self.line,
            .column = self.column,
        };
    }

    fn make_error_token(self: *Self, comptime message: []const u8) Token {
        return .{
            .token_type = TokenType.ERROR,
            .token = message,
            .line = self.line,
            .column = self.column,
        };
    }
};

pub const Token = struct {
    token_type: TokenType,
    token: []const u8,
    line: usize,
    column: usize,
};

pub const TokenType = enum {
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,
    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    ERROR,
    EOF,
};
