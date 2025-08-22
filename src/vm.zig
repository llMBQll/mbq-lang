const std = @import("std");

const chunks = @import("chunks.zig");
const context = @import("context.zig");
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

const STACK_MAX = 256;

pub const InterpretError = error{
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    const Self = @This();

    chunk: ?*Chunk,
    ip: usize,
    stack: FixedStack(Value, STACK_MAX),
    obj_list: ?*Obj,
    globals: Table,
    strings: Table,
    ctx: context.Context,

    pub fn init(ctx: context.Context) !Self {
        return Self{
            .chunk = null,
            .ip = 0,
            .stack = FixedStack(Value, STACK_MAX).init(),
            .obj_list = null,
            .globals = Table.init(ctx.allocator),
            .strings = Table.init(ctx.allocator),
            .ctx = ctx,
        };
    }

    pub fn deinit(self: *Self) void {
        self.globals.deinit();
        self.strings.deinit();

        var current = self.obj_list;
        while (current) |obj| {
            const next = obj.next;
            objects.deallocate(self, obj);
            current = next;
        }
    }

    pub fn interpret(self: *Self, source: []const u8) !void {
        var chunk = try chunks.Chunk.init(self.ctx.allocator);
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
        const stdout = self.ctx.stdout;

        while (true) {
            if (comptime DEBUG_TRACING) {
                try stdout.print("          ", .{});
                if (self.stack.len == 0) {
                    try stdout.print("[ ]", .{});
                } else {
                    for (0..self.stack.len) |i| {
                        try stdout.print("[ ", .{});
                        try self.stack.items[i].print(stdout);
                        try stdout.print(" ]", .{});
                    }
                }
                try stdout.print("\n", .{});

                _ = try debug.disassemble_instruction(self.chunk.?, self.ip, stdout);
            }

            const instruction: OpCode = @enumFromInt(self.read_byte());
            switch (instruction) {
                OpCode.CONSTANT => {
                    const constant = read_constant(self);
                    self.stack.push(constant);
                },
                OpCode.NIL => self.stack.push(.nil),
                OpCode.TRUE => self.stack.push(.{ .bool = true }),
                OpCode.FALSE => self.stack.push(.{ .bool = false }),
                OpCode.POP => _ = self.stack.pop(),
                OpCode.GET_LOCAL => {
                    const slot = self.read_byte();
                    self.stack.push(self.stack.items[slot]);
                },
                OpCode.SET_LOCAL => {
                    const slot = self.read_byte();
                    self.stack.items[slot] = self.stack.peek(0).*;
                },
                OpCode.GET_GLOBAL => {
                    const name: *String = @ptrCast(self.read_constant().object);
                    if (self.globals.get(name)) |value| {
                        self.stack.push(value);
                    } else {
                        try self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                        return InterpretError.RUNTIME_ERROR;
                    }
                },
                OpCode.DEFINE_GLOBAL => {
                    const name: *String = @ptrCast(self.read_constant().object);
                    _ = try self.globals.set(name, self.stack.pop());
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
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(.{ .bool = a.equals(b) });
                },
                OpCode.GREATER => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .bool = a > b });
                },
                OpCode.LESS => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .bool = a < b });
                },
                OpCode.NEGATE => {
                    if (self.stack.peek(0).tag() != ValueType.number) {
                        try self.runtime_error("Operand must be a number.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    self.stack.push(.{ .number = -self.stack.pop().number });
                },
                OpCode.ADD => {
                    if (objects.is(self.stack.peek(0), ObjType.STRING) and objects.is(self.stack.peek(1), ObjType.STRING)) {
                        try self.concatenate();
                    } else if (self.stack.peek(0).tag() == ValueType.number and self.stack.peek(1).tag() == ValueType.number) {
                        const b = self.stack.pop().number;
                        const a = self.stack.pop().number;
                        self.stack.push(.{ .number = a + b });
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
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .number = a - b });
                },
                OpCode.MULTIPLY => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .number = a * b });
                },
                OpCode.DIVIDE => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        try self.runtime_error("Operands must be numbers.", .{});
                        return InterpretError.RUNTIME_ERROR;
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .number = a / b });
                },
                OpCode.NOT => {
                    self.stack.push(.{ .bool = self.stack.pop().falsey() });
                },
                OpCode.PRINT => {
                    try self.stack.pop().print(stdout);
                    try stdout.print("\n", .{});
                },
                OpCode.JUMP => {
                    const offset = self.read_short();
                    self.ip += offset;
                },
                OpCode.JUMP_IF_FALSE => {
                    const offset = self.read_short();
                    if (self.stack.peek(0).falsey()) {
                        self.ip += offset;
                    }
                },
                OpCode.LOOP => {
                    const offset = self.read_short();
                    self.ip -= offset;
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

    fn read_short(self: *Self) u16 {
        const high: u16 = self.chunk.?.code.items[self.ip];
        const low: u16 = self.chunk.?.code.items[self.ip + 1];
        self.ip += 2;
        const instruction = (high << 8) + low;
        return instruction;
    }

    fn read_constant(self: *Self) Value {
        const offset = self.read_byte();
        return self.chunk.?.constants.items[offset];
    }

    fn runtime_error(self: *Self, comptime format: []const u8, args: anytype) !void {
        const stderr = self.ctx.stderr;

        try stderr.print(format, args);
    }

    fn concatenate(self: *Self) !void {
        const b: *String = @ptrCast(self.stack.pop().object);
        const a: *String = @ptrCast(self.stack.pop().object);

        const len = a.chars.len + b.chars.len;
        const chars = try self.ctx.allocator.alloc(u8, len);

        std.mem.copyForwards(u8, chars[0..a.chars.len], a.chars);
        std.mem.copyForwards(u8, chars[a.chars.len..len], b.chars);

        const str = try objects.take_string(self, chars);
        self.stack.push(.{ .object = @ptrCast(str) });
    }
};

fn FixedStack(comptime value_type: type, comptime size: usize) type {
    return struct {
        const Self = @This();
        const N = size;
        const T = value_type;

        items: [N]T,
        len: usize,

        pub fn init() Self {
            return .{
                .items = undefined,
                .len = 0,
            };
        }

        pub fn push(self: *Self, value: T) void {
            self.items[self.len] = value;
            self.len += 1;
        }

        pub fn pop(self: *Self) T {
            self.len -= 1;
            return self.items[self.len];
        }

        pub fn top(self: *Self) *T {
            return &self.items[self.len - 1];
        }

        pub fn peek(self: *Self, distance: usize) *T {
            return &self.items[self.len - 1 - distance];
        }

        pub fn reset(self: *Self) void {
            self.len = 0;
        }
    };
}
