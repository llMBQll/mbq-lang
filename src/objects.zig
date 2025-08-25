const std = @import("std");

const chunks = @import("chunks.zig");
const values = @import("values.zig");
const vm_mod = @import("vm.zig");

const Allocator = std.mem.Allocator;
const Chunk = chunks.Chunk;
const Value = values.Value;
const ValueType = values.ValueType;
const VM = vm_mod.VM;

pub const ObjectType = enum {
    CLOSURE,
    FUNCTION,
    NATIVE,
    STRING,
    UPVALUE,
};

pub const Object = struct {
    const Self = @This();

    type: ObjectType,
    next: ?*Object,

    pub fn print(self: *const Self, stdout: anytype) !void {
        switch (self.type) {
            ObjectType.CLOSURE => {
                const closure: *const Closure = @ptrCast(self);
                try print_function(stdout, closure.function);
            },
            ObjectType.FUNCTION => {
                const function: *const Function = @ptrCast(self);
                try print_function(stdout, function);
            },
            ObjectType.NATIVE => {
                // const native: *const Native = @ptrCast(self);
                try stdout.print("<native fn>", .{});
            },
            ObjectType.STRING => {
                const str: *const String = @ptrCast(self);
                try stdout.print("{s}", .{str.chars});
            },
            ObjectType.UPVALUE => {
                // const upvalue: *const Upvalue = @ptrCast(self);
                try stdout.print("upvalue", .{});
            },
        }
    }

    fn print_function(stdout: anytype, function: *const Function) !void {
        if (function.name) |name| {
            try stdout.print("fn <{s}>", .{name.chars});
        } else {
            try stdout.print("<script>", .{});
        }
    }
};

pub const Function = struct {
    const Type = ObjectType.FUNCTION;

    object: Object,
    arity: usize,
    upvalue_count: usize,
    chunk: Chunk,
    name: ?*String,
};

pub const NativeFn = *const fn (args: []Value) Value;

pub const Native = struct {
    const Type = ObjectType.NATIVE;

    object: Object,
    function: NativeFn,
};

pub const String = struct {
    const Type = ObjectType.STRING;

    object: Object,
    chars: []const u8,
    hash: u32,
};

pub const Upvalue = struct {
    const Type = ObjectType.UPVALUE;

    object: Object,
    location: *Value,
    closed: Value,
    next: ?*Upvalue,
};

pub const Closure = struct {
    const Type = ObjectType.CLOSURE;

    object: Object,
    function: *Function,
    upvalues: std.ArrayList(?*Upvalue),
};

pub fn is(value: *Value, @"type": ObjectType) bool {
    return value.tag() == ValueType.object and value.object.type == @"type";
}

pub fn new_function(vm: *VM) !*Function {
    const function = try allocate(vm, Function);
    function.arity = 0;
    function.upvalue_count = 0;
    function.chunk = Chunk.init();
    function.name = null;
    return function;
}

pub fn new_native(vm: *VM, function: NativeFn) !*Native {
    const native = try allocate(vm, Native);
    native.function = function;
    return native;
}

pub fn copy_string(vm: *VM, chars_in: []const u8) !*String {
    const hash = hash_string(chars_in);

    const interned = vm.strings.find_string(chars_in, hash);
    if (interned) |str| {
        return str;
    }

    const chars = try vm.ctx.allocator.alloc(u8, chars_in.len);
    std.mem.copyForwards(u8, chars, chars_in);

    return allocate_string(vm, chars, hash);
}

pub fn take_string(vm: *VM, chars: []const u8) !*String {
    const hash = hash_string(chars);

    const interned = vm.strings.find_string(chars, hash);
    if (interned) |str| {
        vm.ctx.allocator.free(chars);
        return str;
    }

    return allocate_string(vm, chars, hash);
}

fn allocate_string(vm: *VM, chars: []const u8, hash: u32) !*String {
    const str = try allocate(vm, String);
    str.chars = chars;
    str.hash = hash;

    _ = try vm.strings.set(vm.ctx.allocator, str, .nil);

    return str;
}

fn hash_string(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}

pub fn new_closure(vm: *VM, function: *Function) !*Closure {
    const closure = try allocate(vm, Closure);
    closure.function = function;
    closure.upvalues = try std.ArrayList(?*Upvalue).initCapacity(vm.ctx.allocator, function.upvalue_count);
    closure.upvalues.appendNTimesAssumeCapacity(null, function.upvalue_count);
    return closure;
}

pub fn new_upvalue(vm: *VM, slot: *Value) !*Upvalue {
    const upvalue = try allocate(vm, Upvalue);
    upvalue.closed = .nil;
    upvalue.location = slot;
    upvalue.next = null;
    return upvalue;
}

fn allocate(vm: *VM, comptime T: type) !*T {
    const object = try vm.ctx.allocator.create(T);

    object.object.type = T.Type;
    object.object.next = vm.object_list;
    vm.object_list = @ptrCast(object);

    return object;
}

pub fn deallocate(vm: *VM, obj: *Object) void {
    const allocator = vm.ctx.allocator;

    switch (obj.type) {
        ObjectType.CLOSURE => {
            const closure: *Closure = @ptrCast(obj);
            closure.upvalues.deinit(allocator);
            allocator.destroy(closure);
        },
        ObjectType.FUNCTION => {
            const function: *Function = @ptrCast(obj);
            function.chunk.deinit(allocator);
            allocator.destroy(function);
        },
        ObjectType.NATIVE => {
            const native: *Native = @ptrCast(obj);
            allocator.destroy(native);
        },
        ObjectType.STRING => {
            const str: *String = @ptrCast(obj);
            allocator.free(str.chars);
            allocator.destroy(str);
        },
        ObjectType.UPVALUE => {
            const upvalue: *Upvalue = @ptrCast(obj);
            allocator.destroy(upvalue);
        },
    }
}
