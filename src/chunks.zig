const std = @import("std");

const array = @import("array.zig");
const memory = @import("memory.zig");
const values = @import("values.zig");

const Allocator = std.mem.Allocator;
const Array = array.Array;
const Memory = memory.Memory;
const x = std.ArrayList;

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

    code: Array(u8),
    lines: Array(u32),
    constants: Array(values.Value),

    pub fn init() Self {
        return Self{
            .code = Array(u8).empty,
            .lines = Array(u32).empty,
            .constants = Array(values.Value).empty,
        };
    }

    pub fn deinit(self: *Self, mem: *Memory) void {
        self.code.deinit(mem);
        self.lines.deinit(mem);
        self.constants.deinit(mem);
    }

    pub fn write_byte(self: *Self, mem: *Memory, val: anytype, line: usize) !void {
        const T = @TypeOf(val);
        const byte: u8 = switch (T) {
            u1, u8 => val,
            u18, u32, u64, u128, usize => @truncate(val),
            comptime_int => @truncate(val),
            OpCode => @intFromEnum(val),
            else => @compileError("Type must be an unsigned integer or chunks.OpCode, got " ++ @typeName(T)),
        };
        try self.code.append(mem, byte);
        try self.lines.append(mem, @truncate(line));
    }

    pub fn add_constant(self: *Self, mem: *Memory, val: values.Value) !usize {
        try self.constants.append(mem, val);
        const offset = self.constants.items.len - 1;
        return offset;
    }
};
