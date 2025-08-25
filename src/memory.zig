const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn allocate(allocator: Allocator, comptime T: type, n: usize) Allocator.Error![]T {
    return reallocate(allocator, T, null, n);
}

pub fn free(allocator: Allocator, comptime T: type, mem: []T) void {
    _ = reallocate(allocator, T, mem, 0) catch {};
}

pub fn grow_capacity(n: usize) usize {
    return if (n < 8) 8 else n * 2;
}

pub fn grow_array(allocator: Allocator, comptime T: type, mem: []T, new_size: usize) Allocator.Error![]T {
    return reallocate(allocator, T, mem, new_size);
}

pub fn create(allocator: Allocator, comptime T: type) Allocator.Error!*T {
    const new = try reallocate(allocator, T, null, 1);
    return @ptrCast(new.ptr);
}

pub fn destroy(allocator: Allocator, pointer: anytype) void {
    const pointer_type = @typeInfo(pointer);
    const value_type = pointer_type.pointer.child;

    _ = reallocate(allocator, value_type, @ptrCast(pointer), 0) catch {};
}

fn reallocate(
    allocator: Allocator,
    comptime T: type,
    mem: ?[]T,
    size: usize,
) Allocator.Error![]T {
    if (mem) |m| {
        return try allocator.realloc(m, size);
    } else {
        return try allocator.alloc(T, size);
    }
}
