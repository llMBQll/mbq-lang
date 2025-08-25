const std = @import("std");

const chunks = @import("chunks.zig");
const values = @import("values.zig");
const vm_mod = @import("vm.zig");

const Allocator = std.mem.Allocator;
const Chunk = chunks.Chunk;
const Value = values.Value;
const ValueType = values.ValueType;
const VM = vm_mod.VM;

pub const ObjType = enum {
    CLOSURE,
    FUNCTION,
    NATIVE,
    STRING,
    UPVALUE,
};

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    next: ?*Obj,

    pub fn print(self: *const Self, stdout: anytype) !void {
        switch (self.obj_type) {
            ObjType.CLOSURE => {
                const closure: *const Closure = @ptrCast(self);
                try print_function(stdout, closure.function);
            },
            ObjType.FUNCTION => {
                const function: *const Function = @ptrCast(self);
                try print_function(stdout, function);
            },
            ObjType.NATIVE => {
                // const native: *const Native = @ptrCast(self);
                try stdout.print("<native fn>", .{});
            },
            ObjType.STRING => {
                const str: *const String = @ptrCast(self);
                try stdout.print("{s}", .{str.chars});
            },
            ObjType.UPVALUE => {
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
    const obj_type = ObjType.FUNCTION;

    obj: Obj,
    arity: usize,
    upvalue_count: usize,
    chunk: Chunk,
    name: ?*String,
};

pub const NativeFn = *const fn (args: []Value) Value;

pub const Native = struct {
    const obj_type = ObjType.NATIVE;

    obj: Obj,
    function: NativeFn,
};

pub const String = struct {
    const obj_type = ObjType.STRING;

    obj: Obj,
    chars: []const u8,
    hash: u32,
};

pub const Upvalue = struct {
    const obj_type = ObjType.UPVALUE;

    obj: Obj,
    location: *Value,
    closed: Value,
    next: ?*Upvalue,
};

pub const Closure = struct {
    const obj_type = ObjType.CLOSURE;

    obj: Obj,
    function: *Function,
    upvalues: std.ArrayList(?*Upvalue),
};

pub fn is(value: *Value, obj_type: ObjType) bool {
    return value.tag() == ValueType.object and value.object.obj_type == obj_type;
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

    object.obj.obj_type = T.obj_type;
    object.obj.next = vm.obj_list;
    vm.obj_list = @ptrCast(object);

    return object;
}

pub fn deallocate(vm: *VM, obj: *Obj) void {
    const allocator = vm.ctx.allocator;

    switch (obj.obj_type) {
        ObjType.CLOSURE => {
            const closure: *Closure = @ptrCast(obj);
            closure.upvalues.deinit(allocator);
            allocator.destroy(closure);
        },
        ObjType.FUNCTION => {
            const function: *Function = @ptrCast(obj);
            function.chunk.deinit(allocator);
            allocator.destroy(function);
        },
        ObjType.NATIVE => {
            const native: *Native = @ptrCast(obj);
            allocator.destroy(native);
        },
        ObjType.STRING => {
            const str: *String = @ptrCast(obj);
            allocator.free(str.chars);
            allocator.destroy(str);
        },
        ObjType.UPVALUE => {
            const upvalue: *Upvalue = @ptrCast(obj);
            allocator.destroy(upvalue);
        },
    }
}

fn hash_string(key: []const u8) u32 {
    var hash: u32 = 2166136261;
    for (key) |c| {
        hash ^= c;
        hash *%= 16777619;
    }
    return hash;
}
