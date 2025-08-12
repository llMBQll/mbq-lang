const std = @import("std");

const chunks = @import("chunks.zig");
const debug = @import("debug.zig");
const VM = @import("vm.zig").VM;

const OpCode = chunks.OpCode;

pub fn main() !void {
    const stdout = std.io.getStdOut().writer();

    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(general_purpose_allocator.deinit() == .ok);

    const allocator = general_purpose_allocator.allocator();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    var argv_iterator = try std.process.argsWithAllocator(allocator);
    defer argv_iterator.deinit();

    var argv = std.ArrayList([:0]const u8).init(allocator);
    defer argv.deinit();

    while (argv_iterator.next()) |arg| {
        try argv.append(arg);
    }

    const argc = argv.items.len;

    if (argc == 1) {
        repl(allocator) catch |err| {
            try stdout.print("{}", .{err});
            std.process.exit(1);
        };
    } else if (argc == 2) {
        run_file(allocator, argv.items[1]) catch |err| {
            try stdout.print("{}", .{err});
            std.process.exit(2);
        };
    } else {
        try stdout.print("Usage: mbq-lang [path]\n", .{});
    }
}

fn repl(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();
    const stdin = std.io.getStdIn().reader();

    var vm = try VM.init(allocator);
    defer vm.deinit();

    var buf: [1024]u8 = undefined;

    while (true) {
        try stdout.print("> ", .{});

        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n') orelse break;
        try vm.interpret(line, allocator);
    }
}

fn run_file(allocator: std.mem.Allocator, path: [:0]const u8) !void {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    var vm = try VM.init(allocator);
    defer vm.deinit();

    return vm.interpret(source, allocator);
}
