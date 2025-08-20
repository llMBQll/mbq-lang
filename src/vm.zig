const std = @import("std");

const chunks = @import("chunks.zig");
const compiler = @import("compiler.zig");
const debug = @import("debug.zig");
const objects = @import("objects.zig");
const table = @import("table.zig");
const values = @import("values.zig");

const Chunk = chunks.Chunk;
const Obj = objects.Obj;
const ObjType = objects.ObjType;
const OpCode = chunks.OpCode;
const String = objects.String;
const Table = table.Table;
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
    allocator: std.mem.Allocator,
    obj_list: ?*Obj,
    globals: Table,
    strings: Table,

    pub fn init(allocator: std.mem.Allocator) !Self {
        return Self{
            .chunk = null,
            .ip = 0,
            .stack = try Stack.init(allocator),
            .allocator = allocator,
            .obj_list = null,
            .globals = Table.init(allocator),
            .strings = Table.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit();
        self.strings.deinit();
        self.stack.deinit();

        var current = self.obj_list;
        while (current) |obj| {
            const next = obj.next;
            objects.deallocate(self, obj);
            current = next;
        }
    }

    pub fn interpret(self: *Self, source: []const u8, allocator: std.mem.Allocator) !void {
        var chunk = try chunks.Chunk.init(allocator);
        defer chunk.deinit();

        const ok = try compiler.compile(self, source, &chunk);
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
                        try value.print();
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
                OpCode.POP => _ = self.stack.pop(),
                OpCode.GET_GLOBAL => {
                    const name: *String = @ptrCast(self.read_constant().object);
                    if (self.globals.get(name)) |value| {
                        try self.stack.push(value);
                    } else {
                        try self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RUNTIME_ERROR;
                    }
                },
                OpCode.DEFINE_GLOBAL => {
                    const name: *String = @ptrCast(self.read_constant().object);
                    _ = try self.globals.set(name, self.stack.pop().?);
                },
                OpCode.SET_GLOBAL => {
                    const name: *String = @ptrCast(self.read_constant().object);
                    const is_new = try self.globals.set(name, self.stack.peek(0).*);
                    if (is_new) {
                        _ = self.globals.delete(name);
                        try self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RUNTIME_ERROR;
                    }
                },
                OpCode.EQUAL => {
                    const b = self.stack.pop().?;
                    const a = self.stack.pop().?;
                    try self.stack.push(.{ .bool = a.equals(b) });
                },
                OpCode.GREATER => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .bool = a > b });
                },
                OpCode.LESS => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .bool = a < b });
                },
                OpCode.NEGATE => {
                    if (self.stack.peek(0).tag() != ValueType.number) {
                        try self.runtime_error("Operand must be a number.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    try self.stack.push(.{ .number = -self.stack.pop().?.number });
                },
                OpCode.ADD => {
                    if (objects.is(self.stack.peek(0), ObjType.STRING) and objects.is(self.stack.peek(1), ObjType.STRING)) {
                        try self.concatenate();
                    } else if (self.stack.peek(0).tag() == ValueType.number and self.stack.peek(1).tag() == ValueType.number) {
                        const b = self.stack.pop().?.number;
                        const a = self.stack.pop().?.number;
                        try self.stack.push(.{ .number = a + b });
                    } else {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                },
                OpCode.SUBTRACT => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a - b });
                },
                OpCode.MULTIPLY => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a * b });
                },
                OpCode.DIVIDE => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().?.number;
                    const a = self.stack.pop().?.number;
                    try self.stack.push(.{ .number = a / b });
                },
                OpCode.NOT => {
                    try self.stack.push(.{ .bool = self.stack.pop().?.falsey() });
                },
                OpCode.PRINT => {
                    try self.stack.pop().?.print();
                    try stdout.print("\n", .{});
                },
                OpCode.RETURN => {
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

    fn runtime_error(self: *Self, comptime format: []const u8, args: anytype) !void {
        _ = self;

        const stderr = std.io.getStdErr().writer();

        try stderr.print(format, args);
    }

    fn concatenate(self: *Self) !void {
        const b: *String = @ptrCast(self.stack.pop().?.object);
        const a: *String = @ptrCast(self.stack.pop().?.object);

        const len = a.chars.len + b.chars.len;
        const chars = try self.allocator.alloc(u8, len);

        std.mem.copyForwards(u8, chars[0..a.chars.len], a.chars);
        std.mem.copyForwards(u8, chars[a.chars.len..len], b.chars);

        const str = try objects.take_string(self, chars);
        try self.stack.push(.{ .object = @ptrCast(str) });
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
