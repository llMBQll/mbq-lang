const std = @import("std");

const objects = @import("objects.zig");

const Obj = objects.Obj;
const String = objects.String;

pub const ValueType = enum {
    nil,
    bool,
    number,
    object,
};

pub const Value = union(ValueType) {
    pub const Self = @This();

    nil,
    bool: bool,
    number: f64,
    object: *Obj,

    pub fn tag(self: Self) ValueType {
        switch (self) {
            .nil => return ValueType.nil,
            .bool => return ValueType.bool,
            .number => return ValueType.number,
            .object => return ValueType.object,
        }
    }

    pub fn print(self: Self) !void {
        const stdout = std.io.getStdOut().writer();

        switch (self) {
            .nil => try stdout.print("nil", .{}),
            .bool => |v| try stdout.print("{}", .{v}),
            .number => |v| try stdout.print("{d}", .{v}),
            .object => |v| try v.print(),
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
            .object => return true,
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
            .object => |v| {
                const lhs: *String = @ptrCast(v);
                const rhs: *String = @ptrCast(other.object);
                return std.mem.eql(u8, lhs.chars, rhs.chars);
            },
        }
    }
};
