const std = @import("std");

const chunks = @import("chunks.zig");
const context = @import("context.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

const OpCode = chunks.OpCode;

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

    const ctx = context.Context{
        .stdin = stdin,
        .stdout = stdout,
        .stderr = stderr,
        .allocator = allocator,
    };

    var argv_iterator = try std.process.argsWithAllocator(allocator);
    defer argv_iterator.deinit();

    var argv = std.ArrayList([:0]const u8).empty;
    defer argv.deinit(allocator);

    while (argv_iterator.next()) |arg| {
        try argv.append(allocator, arg);
    }

    const argc = argv.items.len;

    if (argc == 1) {
        repl(ctx) catch |err| {
            try stdout.print("{}", .{err});
            std.process.exit(1);
        };
    } else if (argc == 2) {
        run_file(ctx, argv.items[1]) catch |err| {
            try stdout.print("{}", .{err});
            std.process.exit(2);
        };
    } else {
        try stdout.print("Usage: mbq-lang [path]\n", .{});
    }
}

fn repl(ctx: context.Context) !void {
    var vm = try VM.init(ctx);
    defer vm.deinit();

    const stdin = ctx.stdin;
    const stdout = ctx.stdout;
    const stderr = ctx.stderr;

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

fn run_file(ctx: context.Context, path: [:0]const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(ctx.allocator, std.math.maxInt(usize));
    defer ctx.allocator.free(source);

    var vm = try VM.init(ctx);
    defer vm.deinit();

    return vm.interpret(source);
}
