const std = @import("std");

const values = @import("values.zig");

const Allocator = std.mem.Allocator;

pub const OpCode = enum(u8) {
    CONSTANT,
    NIL,
    TRUE,
    FALSE,
    POP,
    GET_LOCAL,
    SET_LOCAL,
    GET_GLOBAL,
    DEFINE_GLOBAL,
    SET_GLOBAL,
    GET_UPVALUE,
    SET_UPVALUE,
    EQUAL,
    GREATER,
    LESS,
    NEGATE,
    ADD,
    SUBTRACT,
    MULTIPLY,
    DIVIDE,
    NOT,
    PRINT,
    JUMP,
    JUMP_IF_FALSE,
    LOOP,
    CALL,
    CLOSURE,
    CLOSE_UPVALUE,
    RETURN,
};

pub const Chunk = struct {
    const Self = @This();

    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(values.Value),

    pub fn init() Self {
        return Self{
            .code = std.ArrayList(u8).empty,
            .lines = std.ArrayList(u32).empty,
            .constants = std.ArrayList(values.Value).empty,
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.code.deinit(allocator);
        self.lines.deinit(allocator);
        self.constants.deinit(allocator);
    }

    pub fn write_byte(self: *Self, allocator: Allocator, val: anytype, line: usize) !void {
        const T = @TypeOf(val);
        const byte: u8 = switch (T) {
            u1, u8 => val,
            u18, u32, u64, u128, usize => @truncate(val),
            comptime_int => @truncate(val),
            OpCode => @intFromEnum(val),
            else => @compileError("Type must be an unsigned integer or chunks.OpCode, got " ++ @typeName(T)),
        };
        try self.code.append(allocator, byte);
        try self.lines.append(allocator, @truncate(line));
    }

    pub fn add_constant(self: *Self, allocator: Allocator, val: values.Value) !usize {
        try self.constants.append(allocator, val);
        const offset = self.constants.items.len - 1;
        return offset;
    }
};
