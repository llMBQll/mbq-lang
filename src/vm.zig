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
    const FrameStack = FixedStack(CallFrame, FRAMES_MAX);
    const Stack = FixedStack(Value, STACK_MAX);

    frames: FixedStack(CallFrame, FRAMES_MAX),
    stack: FixedStack(Value, STACK_MAX),
    chunk: ?*Chunk,
    obj_list: ?*Obj,
    globals: Table,
    strings: Table,
    ctx: context.Context,

    pub fn init(ctx: context.Context) !Self {
        var self = Self{
            .frames = FixedStack(CallFrame, FRAMES_MAX).init(),
            .stack = FixedStack(Value, STACK_MAX).init(),
            .chunk = null,
            .obj_list = null,
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
            try self.stack_op(Stack.push, .{ &self.stack, Value{ .object = @ptrCast(function) } });

            _ = try self.call(function, 0);

            return self.run();
        } else {
            return InterpretError.COMPILE_ERROR;
        }
    }

    fn run(self: *Self) !void {
        const stdout = self.ctx.stdout;

        var frame = try self.stack_op(FrameStack.top, .{&self.frames});

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

                _ = try debug.disassemble_instruction(&frame.function.chunk, frame.ip, stdout);
            }

            const instruction: OpCode = @enumFromInt(read_byte(frame));
            switch (instruction) {
                OpCode.CONSTANT => {
                    const constant = read_constant(frame);
                    try self.stack_op(Stack.push, .{ &self.stack, constant });
                },
                OpCode.NIL => try self.stack_op(Stack.push, .{ &self.stack, .nil }),
                OpCode.TRUE => try self.stack_op(Stack.push, .{ &self.stack, Value{ .bool = true } }),
                OpCode.FALSE => try self.stack_op(Stack.push, .{ &self.stack, Value{ .bool = false } }),
                OpCode.POP => _ = try self.stack_op(Stack.pop, .{&self.stack}),
                OpCode.GET_LOCAL => {
                    const slot = read_byte(frame);
                    try self.stack_op(Stack.push, .{ &self.stack, frame.slots[slot] });
                },
                OpCode.SET_LOCAL => {
                    const slot = read_byte(frame);
                    frame.slots[slot] = (try self.stack_op(Stack.peek, .{ &self.stack, 0 })).*;
                },
                OpCode.GET_GLOBAL => {
                    const name: *String = @ptrCast(read_constant(frame).object);
                    if (self.globals.get(name)) |value| {
                        try self.stack_op(Stack.push, .{ &self.stack, value });
                    } else {
                        return self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                OpCode.DEFINE_GLOBAL => {
                    const name: *String = @ptrCast(read_constant(frame).object);
                    _ = try self.globals.set(name, try self.stack_op(Stack.pop, .{&self.stack}));
                },
                OpCode.SET_GLOBAL => {
                    const name: *String = @ptrCast(read_constant(frame).object);
                    const is_new = try self.globals.set(name, (try self.stack_op(Stack.peek, .{ &self.stack, 0 })).*);
                    if (is_new) {
                        _ = self.globals.delete(name);
                        return self.runtime_error("Undefined variable '{s}'.", .{name.chars});
                    }
                },
                OpCode.EQUAL => {
                    const b = try self.stack_op(Stack.pop, .{&self.stack});
                    const a = try self.stack_op(Stack.pop, .{&self.stack});
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .bool = a.equals(b) } });
                },
                OpCode.GREATER => {
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() != ValueType.number or (try self.stack_op(Stack.peek, .{ &self.stack, 1 })).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    const a = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .bool = a > b } });
                },
                OpCode.LESS => {
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() != ValueType.number or (try self.stack_op(Stack.peek, .{ &self.stack, 1 })).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    const a = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .bool = a < b } });
                },
                OpCode.NEGATE => {
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() != ValueType.number) {
                        return self.runtime_error("Operand must be a number.", .{});
                    }
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .number = -(try self.stack_op(Stack.pop, .{&self.stack})).number } });
                },
                OpCode.ADD => {
                    if (objects.is(try self.stack_op(Stack.peek, .{ &self.stack, 0 }), ObjType.STRING) and objects.is(try self.stack_op(Stack.peek, .{ &self.stack, 1 }), ObjType.STRING)) {
                        try self.concatenate();
                    } else if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() == ValueType.number and (try self.stack_op(Stack.peek, .{ &self.stack, 1 })).tag() == ValueType.number) {
                        const b = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                        const a = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                        try self.stack_op(Stack.push, .{ &self.stack, Value{ .number = a + b } });
                    } else {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                },
                OpCode.SUBTRACT => {
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() != ValueType.number or (try self.stack_op(Stack.peek, .{ &self.stack, 1 })).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    const a = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .number = a - b } });
                },
                OpCode.MULTIPLY => {
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() != ValueType.number or (try self.stack_op(Stack.peek, .{ &self.stack, 1 })).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    const a = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .number = a * b } });
                },
                OpCode.DIVIDE => {
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).tag() != ValueType.number or (try self.stack_op(Stack.peek, .{ &self.stack, 1 })).tag() != ValueType.number) {
                        return self.runtime_error("Operands must be numbers.", .{});
                    }
                    const b = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    const a = (try self.stack_op(Stack.pop, .{&self.stack})).number;
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .number = a / b } });
                },
                OpCode.NOT => {
                    try self.stack_op(Stack.push, .{ &self.stack, Value{ .bool = (try self.stack_op(Stack.pop, .{&self.stack})).falsey() } });
                },
                OpCode.PRINT => {
                    const v = try self.stack_op(Stack.pop, .{&self.stack});
                    try v.print(stdout);
                    try stdout.print("\n", .{});
                },
                OpCode.JUMP => {
                    const offset = read_short(frame);
                    frame.ip += offset;
                },
                OpCode.JUMP_IF_FALSE => {
                    const offset = read_short(frame);
                    if ((try self.stack_op(Stack.peek, .{ &self.stack, 0 })).falsey()) {
                        frame.ip += offset;
                    }
                },
                OpCode.LOOP => {
                    const offset = read_short(frame);
                    frame.ip -= offset;
                },
                OpCode.CALL => {
                    const arg_count = read_byte(frame);
                    if (!try self.call_value((try self.stack_op(Stack.peek, .{ &self.stack, arg_count })).*, arg_count)) {
                        return InterpretError.RUNTIME_ERROR;
                    }
                    frame = try self.stack_op(FrameStack.top, .{&self.frames});
                },
                OpCode.RETURN => {
                    const result = try self.stack_op(Stack.pop, .{&self.stack});
                    _ = try self.stack_op(FrameStack.pop, .{&self.frames});
                    if (self.frames.len == 0) {
                        _ = try self.stack_op(Stack.pop, .{&self.stack});
                        return;
                    }

                    // Restore stack to the value before current function call
                    self.stack.len = @TypeOf(self.stack).N - frame.slots.len;
                    try self.stack_op(Stack.push, .{ &self.stack, result });
                    frame = try self.stack_op(FrameStack.top, .{&self.frames});
                },
            }
        }
    }

    fn read_byte(frame: *CallFrame) u8 {
        const instruction: u8 = frame.function.chunk.code.items[frame.ip];
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
        return frame.function.chunk.constants.items[offset];
    }

    fn runtime_error(self: *Self, comptime format: []const u8, args: anytype) InterpretError {
        const stderr = self.ctx.stderr;

        stderr.print(format, args) catch {};
        stderr.print("\n", .{}) catch {};

        var i = self.frames.len;
        while (i > 0) {
            i -= 1;

            const frame = self.frames.items[i];
            const function = frame.function;

            stderr.print("[line {d}] in ", .{function.chunk.lines.items[frame.ip]}) catch {};
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
        const b_val = try self.stack_op(Stack.pop, .{&self.stack});
        const b: *String = @ptrCast(b_val.object);
        const a_val = try self.stack_op(Stack.pop, .{&self.stack});
        const a: *String = @ptrCast(a_val.object);

        const len = a.chars.len + b.chars.len;
        const chars = try self.ctx.allocator.alloc(u8, len);

        std.mem.copyForwards(u8, chars[0..a.chars.len], a.chars);
        std.mem.copyForwards(u8, chars[a.chars.len..len], b.chars);

        const str = try objects.take_string(self, chars);
        try self.stack_op(Stack.push, .{ &self.stack, Value{ .object = @ptrCast(str) } });
    }

    fn define_native(self: *Self, name: []const u8, function: NativeFn) !void {
        const str = try objects.copy_string(self, name);
        const func = try objects.new_native(self, function);

        try self.stack_op(Stack.push, .{ &self.stack, Value{ .object = @ptrCast(str) } });
        try self.stack_op(Stack.push, .{ &self.stack, Value{ .object = @ptrCast(func) } });

        _ = try self.globals.set(str, (try self.stack_op(Stack.peek, .{ &self.stack, 0 })).*);

        _ = try self.stack_op(Stack.pop, .{&self.stack});
        _ = try self.stack_op(Stack.pop, .{&self.stack});
    }

    fn call_value(self: *Self, callee: Value, arg_count: u8) !bool {
        if (callee.tag() == ValueType.object) {
            switch (callee.object.obj_type) {
                ObjType.FUNCTION => return self.call(@ptrCast(callee.object), arg_count),
                ObjType.NATIVE => {
                    const native: *Native = @ptrCast(callee.object);
                    const stack_top = self.stack.len;
                    const result = native.function(self.stack.items[stack_top - arg_count .. stack_top]);
                    self.stack.len -= arg_count + 1;
                    try self.stack_op(Stack.push, .{ &self.stack, result });
                    return true;
                },
                else => {},
            }
        }

        self.runtime_error("Can only call functions and classes.", .{}) catch {};
        return false;
    }

    fn call(self: *Self, function: *Function, arg_count: u8) !bool {
        if (arg_count != function.arity) {
            self.runtime_error("Expected {d} arguments, got {}", .{ function.arity, arg_count }) catch {};
            return false;
        }

        const frame = try self.stack_op(FrameStack.push_new, .{&self.frames});
        frame.function = function;
        frame.ip = 0;
        // Get a view to the stack that starts with all arguments and function call
        frame.slots = self.stack.items[self.stack.len - arg_count - 1 .. @TypeOf(self.stack).N];
        return true;
    }

    fn stack_op(self: *Self, func: anytype, args: anytype) !StackFnWrapper(@TypeOf(func)).Ret {
        if (comptime StackFnWrapper(@TypeOf(func)).Fallible) {
            return @call(.auto, func, args) catch |err| {
                const stack = args.@"0";
                return self.runtime_error("{} Stack [{d}/{d}]", .{ err, stack.len, @TypeOf(stack.*).N });
            };
        } else {
            return @call(.auto, func, args);
        }
    }
};

fn clock_native(_: []Value) Value {
    return .{ .number = @floatFromInt(std.time.milliTimestamp()) };
}

const CallFrame = struct {
    function: *Function,
    ip: usize,
    slots: []Value,
};

const StackError = error{
OutOfRange,
Overflow,
Underflow,
};

fn FixedStack(comptime value_type: type, comptime size: usize) type {
    // TODO add error handling for stack overflow and removal on empty stack

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

        pub fn push(self: *Self, value: T) StackError!void {
            if (self.len == N) {
                return StackError.Overflow;
            }
            self.items[self.len] = value;
            self.len += 1;
        }

        pub fn push_new(self: *Self) StackError!*T {
            if (self.len == N) {
                return StackError.Overflow;
            }
            self.len += 1;
            return self.top();
        }

        pub fn pop(self: *Self) StackError!T {
            if (self.len == 0) {
                return StackError.Underflow;
            }
            self.len -= 1;
            return self.items[self.len];
        }

        pub fn top(self: *Self) StackError!*T {
            if (self.len == 0) {
                return StackError.OutOfRange;
            }
            return &self.items[self.len - 1];
        }

        pub fn peek(self: *Self, distance: usize) StackError!*T {
            if (self.len < distance + 1) {
                return StackError.OutOfRange;
            }
            return &self.items[self.len - 1 - distance];
        }

        pub fn reset(self: *Self) void {
            self.len = 0;
        }
    };
}

fn StackFnWrapper(comptime func: type) type {
    const OriginalRetType = @typeInfo(func).@"fn".return_type.?;
    const IsFallible, const RetType = switch (@typeInfo(OriginalRetType)) {
        .error_union => |e| .{ true, e.payload },
        else => .{ false, OriginalRetType },
    };

    return struct {
        const Ret = RetType;
        const Fallible = IsFallible;
    };
}
