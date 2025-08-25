const std = @import("std");

pub const Context = struct {
    stdin: *std.Io.Reader,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    allocator: std.mem.Allocator,
};
