const std = @import("std");

pub const Context = struct {
    stdout: std.fs.File.Writer,
    stderr: std.fs.File.Writer,
    allocator: std.mem.Allocator,
};
