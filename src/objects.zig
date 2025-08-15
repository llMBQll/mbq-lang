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
};

pub fn is(value: *Value, obj_type: ObjType) bool {
    return value.tag() == ValueType.object and value.object.obj_type == obj_type;
}

pub fn copy_string(vm: *VM, chars_in: []const u8) !*String {
    const chars = try vm.allocator.alloc(u8, chars_in.len);
    std.mem.copyForwards(u8, chars, chars_in);

    const str = try allocate(vm, String);
    str.chars = chars;

    return str;
}

pub fn take_string(vm: *VM, chars_in: []const u8) !*String {
    const str = try allocate(vm, String);
    str.chars = chars_in;
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
