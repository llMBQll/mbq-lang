const std = @import("std");

const chunks = @import("chunks.zig");
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const values = @import("values.zig");

const Chunk = chunks.Chunk;
const OpCode = chunks.OpCode;
const Value = values.Value;
const ValueType = values.ValueType;

const DEBUG_TRACING = true;

pub const InterpretError = error{
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    const Self = @This();

    chunk: ?*Chunk,
    ip: usize,
    stack: Stack,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{
            .chunk = null,
            .ip = 0,
            .stack = try Stack.init(allocator),
        };
    }

    pub fn deinit(self: Self) void {
        self.stack.deinit();
    }

    pub fn interpret(self: *Self, source: []const u8, allocator: std.mem.Allocator) !void {
        var chunk = try chunks.Chunk.init(allocator);
        defer chunk.deinit();

        const ok = try compiler.compile(source, &chunk);
        if (!ok) {
            return InterpretError.COMPILE_ERROR;
        }

        self.chunk = &chunk;
        self.ip = 0;

        return self.run();
    }

    fn run(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        while (true) {
            if (comptime DEBUG_TRACING) {
                try stdout.print("          ", .{});
                if (self.stack.data.items.len == 0) {
                    try stdout.print("[ ]", .{});
                } else {
                    for (self.stack.data.items) |value| {
                        try stdout.print("[ ", .{});
                        try values.print_value(value);
                        try stdout.print(" ]", .{});
                    }
                }
                try stdout.print("\n", .{});

                _ = try debug.disassemble_instruction(self.chunk.?, self.ip);
            }

            const instruction: OpCode = @enumFromInt(self.read_byte());
            switch (instruction) {
                OpCode.CONSTANT => {
                    const constant = read_constant(self);
                    try self.stack.push(constant);
                },
                OpCode.NIL => try self.stack.push(.nil),
                OpCode.TRUE => try self.stack.push(.{ .bool = true }),
                OpCode.FALSE => try self.stack.push(.{ .bool = false }),
                OpCode.EQUAL => {
                    const b = self.stack.pop().?;
                    const a = self.stack.pop().?;
                    try self.stack.push(.{ .bool = values_equal(a, b) });
                },
                OpCode.GREATER => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .bool = a > b });
                },
                OpCode.LESS => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .bool = a < b });
                },
                OpCode.NEGATE => {
                    if (self.stack.peek(0).tag() != ValueType.number) {
                        try self.runtime_error("Operand must be a number.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    try self.stack.push(.{ .number = -self.stack.pop().?.number });
                },
                OpCode.ADD => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a + b });
                },
                OpCode.SUBTRACT => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a - b });
                },
                OpCode.MULTIPLY => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a * b });
                },
                OpCode.DIVIDE => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.");
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a / b });
                },
                OpCode.NOT => {
                    try self.stack.push(.{ .bool = is_falsey(self.stack.pop().?) });
                },
                OpCode.RETURN => {
                    try values.print_value(self.stack.pop().?);
                    try stdout.print("\n", .{});
                    return;
                },
            }
        }
    }

    fn read_byte(self: *Self) u8 {
        const instruction = self.chunk.?.code.items[self.ip];
        self.ip += 1;
        return instruction;
    }

    fn read_constant(self: *Self) Value {
        const offset = self.read_byte();
        return self.chunk.?.constants.items[offset];
    }

    fn runtime_error(self: *Self, comptime message: []const u8) !void {
        _ = self;

        const stderr = std.io.getStdErr().writer();

        try stderr.print(message, .{});
    }

    fn is_falsey(value: Value) bool {
        switch (value) {
            .nil => return true,
            .bool => |v| return !v,
            .number => |v| return v == 0.0,
        }
    }

    fn values_equal(a: Value, b: Value) bool {
        if (a.tag() != b.tag()) {
            return false;
        }

        switch (a) {
            .nil => return true,
            .bool => |v| return v == b.bool,
            .number => |v| return v == b.number,
        }
    }
};

const Stack = struct {
    const Self = @This();
    const DEFAULT_LENGTH = 256;

    data: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{
            .data = try std.ArrayList(Value).initCapacity(allocator, DEFAULT_LENGTH),
        };
    }

    pub fn deinit(self: Self) void {
        self.data.deinit();
    }

    pub fn push(self: *Self, value: Value) !void {
        try self.data.append(value);
    }

    pub fn pop(self: *Self) ?Value {
        return self.data.pop();
    }

    pub fn top(self: *Self) !*Value {
        return &self.data.items[self.data.items.len - 1];
    }

    pub fn peek(self: *Self, distance: usize) *Value {
        return &self.data.items[self.data.items.len - 1 - distance];
    }

    pub fn reset(self: *Self) !void {
        try self.data.shrinkAndFree(DEFAULT_LENGTH);
    }
};
