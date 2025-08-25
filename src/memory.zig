const std = @import("std");

const Allocator = std.mem.Allocator;

pub const Memory = struct {
    const Self = @This();

    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return .{
            .allocator = allocator,
        };
    }

    pub fn alloc(self: *Self, comptime T: type, n: usize) Allocator.Error![]T {
        return self.allocator.alloc(T, n);
    }

    pub fn free(self: *Self, mem: anytype) void {
        return self.allocator.free(mem);
    }

    pub fn grow_capacity(n: usize) usize {
        return if (n < 8) 8 else n * 2;
    }

    pub fn resize_array(self: *Self, mem: anytype, new_size: usize) Allocator.Error!@TypeOf(mem) {
        return self.allocator.realloc(mem, new_size);
    }

    pub fn create(self: *Self, comptime T: type) Allocator.Error!*T {
        return self.allocator.create(T);
    }

    pub fn destroy(self: *Self, ptr: anytype) void {
        return self.allocator.destroy(ptr);
    }
};
