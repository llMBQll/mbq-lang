const std = @import("std");
const memory = @import("memory.zig");

const Allocator = std.mem.Allocator;
const Memory = memory.Memory;

pub fn Array(comptime ValueType: type) type {
    return struct {
        const Self = @This();
        const T = ValueType;

        items: []T,
        capacity: usize,

        pub const empty = Self{
            .items = &[_]T{},
            .capacity = 0,
        };

        pub fn init_with_capacity(mem: *Memory, capacity: usize) Allocator.Error!Self {
            var self = Self.empty;
            try self.ensure_capacity_exact(mem, capacity);
            return self;
        }

        pub fn deinit(self: *Self, mem: *Memory) void {
            mem.free(self.get_full_slice());
            self.* = undefined;
        }

        pub fn append(self: *Self, mem: *Memory, item: T) Allocator.Error!void {
            try self.ensure_capacity(mem, self.items.len + 1);
            self.append_assume_capacity(item);
        }

        pub fn append_assume_capacity(self: *Self, item: T) void {
            std.debug.assert(self.capacity > self.items.len);

            self.items.len += 1;
            self.items[self.items.len - 1] = item;
        }

        fn ensure_capacity_exact(self: *Self, mem: *Memory, new_capacity: usize) Allocator.Error!void {
            // TODO implement the try remap and then realloc optimization

            if (self.capacity >= new_capacity) {
                return;
            }
            const new = try mem.resize_array(self.get_full_slice(), new_capacity);
            self.items.ptr = new.ptr;
            self.capacity = new_capacity;
        }

        fn ensure_capacity(self: *Self, mem: *Memory, new_capacity: usize) Allocator.Error!void {
            if (self.capacity >= new_capacity) {
                return;
            }
            return self.ensure_capacity_exact(mem, Memory.grow_capacity(new_capacity));
        }

        fn get_full_slice(self: *const Self) []T {
            return self.items.ptr[0..self.capacity];
        }
    };
}
