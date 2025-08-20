const std = @import("std");

const values = @import("values.zig");
const vm_mod = @import("vm.zig");

const Allocator = std.mem.Allocator;
const Value = values.Value;
const ValueType = values.ValueType;
const VM = vm_mod.VM;

pub const ObjType = enum {
    STRING,
};

pub const Obj = struct {
    const Self = @This();

    obj_type: ObjType,
    next: ?*Obj,

    pub fn print(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();

        switch (self.obj_type) {
            ObjType.STRING => {
                const str: *String = @ptrCast(self);
                try stdout.print("{s}", .{str.chars});
            },
        }
    }
};

pub const String = struct {
    const obj_type = ObjType.STRING;

    obj: Obj,
    chars: []const u8,
    hash: u32,
};

pub fn is(value: *Value, obj_type: ObjType) bool {
    return value.tag() == ValueType.object and value.object.obj_type == obj_type;
}

pub fn copy_string(vm: *VM, chars_in: []const u8) !*String {
    const hash = hash_string(chars_in);

    const interned = vm.strings.find_string(chars_in, hash);
    if (interned) |str| {
        return str;
    }

    const chars = try vm.allocator.alloc(u8, chars_in.len);
    std.mem.copyForwards(u8, chars, chars_in);

    return allocate_string(vm, chars, hash);
}

pub fn take_string(vm: *VM, chars: []const u8) !*String {
    const hash = hash_string(chars);

    const interned = vm.strings.find_string(chars, hash);
    if (interned) |str| {
        vm.allocator.free(chars);
        return str;
    }

    return allocate_string(vm, chars, hash);
}

fn allocate_string(vm: *VM, chars: []const u8, hash: u32) !*String {
    const str = try allocate(vm, String);
    str.chars = chars;
    str.hash = hash;

    _ = try vm.strings.set(str, .nil);

    return str;
}

fn allocate(vm: *VM, comptime T: type) !*T {
    const object = try vm.allocator.create(T);

    object.obj.obj_type = T.obj_type;
    object.obj.next = vm.obj_list;
    vm.obj_list = @ptrCast(object);

    return object;
}

pub fn deallocate(vm: *VM, obj: *Obj) void {
    switch (obj.obj_type) {
        ObjType.STRING => {
            const str: *String = @ptrCast(obj);
            vm.allocator.free(str.chars);
            vm.allocator.destroy(str);
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
