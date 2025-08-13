const std = @import("std");

pub const ValueType = enum {
    nil,
    bool,
    number,
};

pub const Value = union(ValueType) {
    pub const Self = @This();

    nil,
    bool: bool,
    number: f64,

    pub fn tag(self: Self) ValueType {
        switch (self) {
            .nil => return ValueType.nil,
            .bool => return ValueType.bool,
            .number => return ValueType.number,
        }
    }

    pub fn print(self: Self) !void {
        const stdout = std.io.getStdOut().writer();

        switch (self) {
            .nil => try stdout.print("nil", .{}),
            .bool => |v| try stdout.print("{}", .{v}),
            .number => |v| try stdout.print("{d}", .{v}),
        }
    }

    pub fn truthy(self: Self) bool {
        return !self.falsey();
    }

    pub fn falsey(self: Self) bool {
        switch (self) {
            .nil => return true,
            .bool => |v| return !v,
            .number => |v| return v == 0.0,
        }
    }

    pub fn equals(self: Self, other: Self) bool {
        if (self.tag() != other.tag()) {
            return false;
        }

        switch (self) {
            .nil => return true,
            .bool => |v| return v == other.bool,
            .number => |v| return v == other.number,
        }
    }
};
