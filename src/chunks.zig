const std = @import("std");

const values = @import("values.zig");

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
    RETURN,
};

pub const Chunk = struct {
    const Self = @This();

    code: std.ArrayList(u8),
    lines: std.ArrayList(u32),
    constants: std.ArrayList(values.Value),

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{
            .code = std.ArrayList(u8).init(allocator),
            .lines = std.ArrayList(u32).init(allocator),
            .constants = std.ArrayList(values.Value).init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.code.deinit();
        self.lines.deinit();
        self.constants.deinit();
    }

    pub fn write_byte(self: *Self, val: anytype, line: usize) !void {
        const T = @TypeOf(val);
        const byte: u8 = switch (T) {
            u8 => val,
            u18, u32, u64, u128, usize => @truncate(val),
            comptime_int => @truncate(val),
            OpCode => @intFromEnum(val),
            else => @compileError("Type must be an unsigned integer or chunks.OpCode, got " ++ @typeName(T)),
        };
        try self.code.append(byte);
        try self.lines.append(@truncate(line));
    }

    pub fn add_constant(self: *Self, val: values.Value) !usize {
        try self.constants.append(val);
        const offset = self.constants.items.len - 1;
        return offset;
    }
};
