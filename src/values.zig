const std = @import("std");

pub const ValueType = enum {
    nil,
    bool,
    number,
};

pub const Value = union(ValueType) {
    nil,
    bool: bool,
    number: f64,

    pub fn tag(self: Value) ValueType {
        switch (self) {
            .nil => return ValueType.nil,
            .bool => return ValueType.bool,
            .number => return ValueType.number,
        }
    }
};

pub fn print_value(value: Value) !void {
    const stdout = std.io.getStdOut().writer();

    switch (value) {
        .nil => try stdout.print("nil", .{}),
        .bool => |v| try stdout.print("{}", .{v}),
        .number => |v| try stdout.print("{d}", .{v}),
    }
}
