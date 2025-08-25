const std = @import("std");

const chunks = @import("chunks.zig");
const debug = @import("debug.zig");
const memory_mod = @import("memory.zig");
const VM = @import("vm.zig").VM;

const OpCode = chunks.OpCode;
const Memory = memory_mod.Memory;

pub fn main() !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;

    var stderr_buffer: [1024]u8 = undefined;
    var stderr_writer = std.fs.File.stderr().writer(&stderr_buffer);
    const stderr = &stderr_writer.interface;

    var stdin_buffer: [1024]u8 = undefined;
    var stdin_writer = std.fs.File.stdin().reader(&stdin_buffer);
    const stdin = &stdin_writer.interface;

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);
    const allocator = general_purpose_allocator.allocator();

    var argv_iterator = try std.process.argsWithAllocator(allocator);
    defer argv_iterator.deinit();

    var argv = std.ArrayList([:0]const u8).empty;
    defer argv.deinit(allocator);

    while (argv_iterator.next()) |arg| {
        try argv.append(allocator, arg);
    }

    const argc = argv.items.len;
    if (argc == 1) {
        repl(stdin, stdout, stderr, allocator) catch |err| {
            try stdout.print("{}", .{err});
            std.process.exit(1);
        };
    } else if (argc == 2) {
        run_file(stdin, stdout, stderr, allocator, argv.items[1]) catch |err| {
            try stdout.print("{}", .{err});
            std.process.exit(2);
        };
    } else {
        try stdout.print("Usage: mbq-lang [path]\n", .{});
    }
}

fn repl(
    stdin: *std.Io.Reader,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    allocator: std.mem.Allocator,
) !void {
    var vm = try VM.init(stdin, stdout, stderr, allocator);
    defer vm.deinit();

    while (true) {
        try stdout.print("> ", .{});
        try stdout.flush();

        const line = stdin.takeDelimiterExclusive('\n') catch |err| {
            const Error = std.io.Reader.DelimiterError;
            try stdout.print("\n", .{});
            try stdout.flush();
            switch (err) {
                Error.ReadFailed, Error.StreamTooLong => {
                    stderr.print("{}\n", .{err}) catch {};
                    stderr.flush() catch {};
                },
                Error.EndOfStream => {},
            }
            break;
        };

        try vm.interpret(line);
    }
}

fn run_file(
    stdin: *std.Io.Reader,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    allocator: std.mem.Allocator,
    path: [:0]const u8,
) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    var vm = try VM.init(stdin, stdout, stderr, allocator);
    defer vm.deinit();

    return vm.interpret(source);
}
