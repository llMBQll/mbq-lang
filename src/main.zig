const std = @import("std");

const chunks = @import("chunks.zig");
const context = @import("context.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

const OpCode = chunks.OpCode;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();
    const stderr = std.io.getStdErr().writer();

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);
    const allocator = general_purpose_allocator.allocator();

    const ctx = context.Context{
        .stdout = stdout,
        .stderr = stderr,
        .allocator = allocator,
    };

    var argv_iterator = try std.process.argsWithAllocator(allocator);
    defer argv_iterator.deinit();

    var argv = std.ArrayList([:0]const u8).init(allocator);
    defer argv.deinit();

    while (argv_iterator.next()) |arg| {
        try argv.append(arg);
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
    const stdin = std.io.getStdIn().reader();

    var vm = try VM.init(ctx);
    defer vm.deinit();

    var buf: [1024]u8 = undefined;

    while (true) {
        try ctx.stdout.print("> ", .{});

        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n') orelse {
            try ctx.stdout.print("\n", .{});
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
