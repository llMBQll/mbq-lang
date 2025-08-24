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
const Closure = objects.Closure;
const Function = objects.Function;
const Native = objects.Native;
const NativeFn = objects.NativeFn;
const OpCode = chunks.OpCode;
const String = objects.String;
const Table = table.Table;
const Value = values.Value;
const ValueType = values.ValueType;

const DEBUG_TRACING = false;

const FRAMES_MAX = 64;
const STACK_MAX = FRAMES_MAX * std.math.maxInt(u8);

pub const InterpretError = error{
    COMPILE_ERROR,
    RUNTIME_ERROR,
};

pub const VM = struct {
    const Self = @This();

    frames: FixedStack(CallFrame, FRAMES_MAX),
    stack: FixedStack(Value, STACK_MAX),
    obj_list: ?*Obj,
    open_upvalues: ?*objects.Upvalue,
    globals: Table,
    strings: Table,
    ctx: context.Context,

    pub fn init(ctx: context.Context) !Self {
        var self = Self{
            .frames = FixedStack(CallFrame, FRAMES_MAX).init(),
            .stack = FixedStack(Value, STACK_MAX).init(),
            .obj_list = null,
            .open_upvalues = null,
            .globals = Table.init(ctx.allocator),
            .strings = Table.init(ctx.allocator),
            .ctx = ctx,
        };

        try self.define_native("clock", clock_native);

        return self;
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
        const func = try compiler.compile(self, source);
        if (func) |function| {
            self.stack.push(.{ .object = @ptrCast(function) });
            const closure = try objects.new_closure(self, function);
            _ = self.stack.pop();
            self.stack.push(.{ .object = @ptrCast(closure) });
            _ = try self.call(closure, 0);

            return self.run();
        } else {
            return InterpretError.COMPILE_ERROR;
        }
    }

    fn run(self: *Self) !void {
        const stdout = self.ctx.stdout;

        var frame = self.frames.top();

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

                const offset = frame.ip - frame.closure.function.chunk.code.items.ptr;
                _ = try debug.disassemble_instruction(&frame.closure.function.chunk, offset, stdout);
            }

            const instruction: OpCode = @enumFromInt(read_byte(frame));
            switch (instruction) {
                OpCode.CONSTANT => {
                    const constant = read_constant(frame);
                    self.stack.push(constant);
                },
                OpCode.NIL => self.stack.push(.nil),
                OpCode.TRUE => self.stack.push(.{ .bool = true }),
                OpCode.FALSE => self.stack.push(.{ .bool = false }),
                OpCode.POP => _ = self.stack.pop(),
                OpCode.GET_LOCAL => {
                    const slot = read_byte(frame);
                    self.stack.push(frame.slots[slot]);
                },
                OpCode.SET_LOCAL => {
                    const slot = read_byte(frame);
                    frame.slots[slot] = self.stack.peek(0).*;
                },
                OpCode.GET_GLOBAL => {
                    const name: *String = @ptrCast(read_constant(frame).object);
                    if (self.globals.get(name)) |value| {
                        self.stack.push(value);
                    } else {
                        return self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                OpCode.DEFINE_GLOBAL => {
                    const name: *String = @ptrCast(read_constant(frame).object);
                    _ = try self.globals.set(name, self.stack.pop());
                },
                OpCode.SET_GLOBAL => {
                    const name: *String = @ptrCast(read_constant(frame).object);
                    const is_new = try self.globals.set(name, self.stack.peek(0).*);
                    if (is_new) {
                        _ = self.globals.delete(name);
                        return self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                OpCode.GET_UPVALUE => {
                    const slot = read_byte(frame);
                    self.stack.push(frame.closure.upvalues.items[slot].?.location.*);
                },
                OpCode.SET_UPVALUE => {
                    const slot = read_byte(frame);
                    frame.closure.upvalues.items[slot].?.location = self.stack.peek(0);
                },
                OpCode.EQUAL => {
                    const b = self.stack.pop();
                    const a = self.stack.pop();
                    self.stack.push(.{ .bool = a.equals(b) });
                },
                OpCode.GREATER => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .bool = a > b });
                },
                OpCode.LESS => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .bool = a < b });
                },
                OpCode.NEGATE => {
                    if (self.stack.peek(0).tag() != ValueType.number) {
                        return self.runtime_error("Operand must be a number.", .{});
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
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                },
                OpCode.SUBTRACT => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .number = a - b });
                },
                OpCode.MULTIPLY => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = self.stack.pop().number;
                    const a = self.stack.pop().number;
                    self.stack.push(.{ .number = a * b });
                },
                OpCode.DIVIDE => {
                    if (self.stack.peek(0).tag() != ValueType.number or self.stack.peek(1).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
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
                    const offset = read_short(frame);
                    frame.ip += offset;
                },
                OpCode.JUMP_IF_FALSE => {
                    const offset = read_short(frame);
                    if (self.stack.peek(0).falsey()) {
                        frame.ip += offset;
                    }
                },
                OpCode.LOOP => {
                    const offset = read_short(frame);
                    frame.ip -= offset;
                },
                OpCode.CALL => {
                    const arg_count = read_byte(frame);
                    try self.call_value(self.stack.peek(arg_count).*, arg_count);
                    frame = self.frames.top();
                },
                OpCode.CLOSURE => {
                    const function: *Function = @ptrCast(read_constant(frame).object);
                    const closure = try objects.new_closure(self, function);
                    self.stack.push(.{ .object = @ptrCast(closure) });

                    for (closure.upvalues.items) |*item| {
                        const is_local = read_byte(frame);
                        const index = read_byte(frame);

                        if (is_local == 1) {
                            item.* = try self.capture_upvalue(&frame.slots[index]);
                        } else {
                            item.* = frame.closure.upvalues.items[index].?;
                        }
                    }
                },
                OpCode.CLOSE_UPVALUE => {
                    self.close_upvalues(self.stack.peek(0));
                    _ = self.stack.pop();
                },
                OpCode.RETURN => {
                    const result = self.stack.pop();
                    self.close_upvalues(&frame.slots[0]);

                    _ = self.frames.pop();
                    if (self.frames.len == 0) {
                        _ = self.stack.pop();
                        return;
                    }

                    // Restore stack to the value before current function call
                    self.stack.len = frame.slots - &self.stack.items;
                    self.stack.push(result);
                    frame = self.frames.top();
                },
            }
        }
    }

    fn read_byte(frame: *CallFrame) u8 {
        const instruction: u8 = frame.ip[0];
        frame.ip += 1;
        return instruction;
    }

    fn read_short(frame: *CallFrame) u16 {
        const high: u16 = read_byte(frame);
        const low: u16 = read_byte(frame);
        const instruction = (high << 8) + low;
        return instruction;
    }

    fn read_constant(frame: *CallFrame) Value {
        const offset = read_byte(frame);
        return frame.closure.function.chunk.constants.items[offset];
    }

    fn runtime_error(self: *Self, comptime format: []const u8, args: anytype) InterpretError {
        const stderr = self.ctx.stderr;

        stderr.print(format, args) catch {};
        stderr.print("\n", .{}) catch {};

        var i = self.frames.len;
        while (i > 0) {
            i -= 1;

            const frame = self.frames.items[i];
            const function = frame.closure.function;
            const instruction = frame.ip - frame.closure.function.chunk.code.items.ptr;

            stderr.print("[line {d}] in ", .{function.chunk.lines.items[instruction]}) catch {};
            if (function.name) |name| {
                stderr.print("{s}()\n", .{name.chars}) catch {};
            } else {
                stderr.print("script\n", .{}) catch {};
            }
        }

        self.stack.reset();

        return InterpretError.RUNTIME_ERROR;
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

    fn define_native(self: *Self, name: []const u8, function: NativeFn) !void {
        const str = try objects.copy_string(self, name);
        const func = try objects.new_native(self, function);

        self.stack.push(.{ .object = @ptrCast(str) });
        self.stack.push(.{ .object = @ptrCast(func) });

        _ = try self.globals.set(str, self.stack.peek(0).*);

        _ = self.stack.pop();
        _ = self.stack.pop();
    }

    fn call_value(self: *Self, callee: Value, arg_count: u8) InterpretError!void {
        if (callee.tag() == ValueType.object) {
            switch (callee.object.obj_type) {
                ObjType.CLOSURE => return self.call(@ptrCast(callee.object), arg_count),
                ObjType.NATIVE => {
                    const native: *Native = @ptrCast(callee.object);
                    const stack_top = self.stack.len;
                    const result = native.function(self.stack.items[stack_top - arg_count .. stack_top]);
                    self.stack.len -= arg_count + 1;
                    self.stack.push(result);
                    return;
                },
                else => {},
            }
        }

        return self.runtime_error("Can only call functions and classes.", .{});
    }

    fn call(self: *Self, closure: *Closure, arg_count: u8) InterpretError!void {
        if (arg_count != closure.function.arity) {
            return self.runtime_error("Expected {d} arguments, got {}", .{ closure.function.arity, arg_count });
        }

        const frame = self.frames.push_new();
        frame.closure = closure;
        frame.ip = closure.function.chunk.code.items.ptr;
        frame.slots = @ptrCast(&self.stack.items[self.stack.len - arg_count - 1]);
    }

    fn capture_upvalue(self: *Self, local: *Value) !*objects.Upvalue {
        var prev: ?*objects.Upvalue = null;
        var current: ?*objects.Upvalue = self.open_upvalues;

        while (current != null and current.?.location - local > 0) {
            prev = current;
            current = current.?.next;
        }

        if (current != null and current.?.location == local) {
            return current.?;
        }

        const upvalue = try objects.new_upvalue(self, local);

        upvalue.next = current;
        if (prev == null) {
            self.open_upvalues = upvalue;
        } else {
            prev.?.next = upvalue;
        }

        return upvalue;
    }

    fn close_upvalues(self: *Self, last: *const Value) void {
        while (self.open_upvalues != null and self.open_upvalues.?.location - last > 0) {
            const upvalue = self.open_upvalues;
            upvalue.?.closed = upvalue.?.location.*;
            upvalue.?.location = &upvalue.?.closed;
            self.open_upvalues = upvalue.?.next;
        }
    }
};

fn clock_native(_: []Value) Value {
    return .{ .number = @floatFromInt(std.time.milliTimestamp()) };
}

const CallFrame = struct {
    closure: *Closure,
    ip: [*]u8,
    slots: [*]Value,
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

        pub fn push_new(self: *Self) *T {
            self.len += 1;
            return self.top();
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
