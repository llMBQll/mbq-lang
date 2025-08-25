const std = @import("std");

const chunks = @import("chunks.zig");
const objects = @import("objects.zig");
const values = @import("values.zig");

const OpCode = chunks.OpCode;
const Function = objects.Function;

pub fn disassemble_chunk(chunk: *chunks.Chunk, name: []const u8, stdout: anytype) !void {
    try stdout.print("== {s} ==\n", .{name});

    var offset: usize = 0;
    while (offset < chunk.code.items.len) {
        offset = try disassemble_instruction(chunk, offset, stdout);
    }

    try stdout.print("== {s} end ==\n", .{name});
    try stdout.flush();
}

pub fn disassemble_instruction(chunk: *chunks.Chunk, offset: usize, stdout: anytype) !usize {
    try stdout.print("{d:0>4} ", .{offset});

    if (offset > 0 and chunk.lines.items[offset] == chunk.lines.items[offset - 1]) {
        try stdout.print("   | ", .{});
    } else {
        try stdout.print("{d:0>4} ", .{chunk.lines.items[offset]});
    }

    const instruction = chunk.code.items[offset];
    switch (instruction) {
        @intFromEnum(OpCode.CONSTANT) => return try constant_instruction("OP_CONSTANT", chunk, offset, stdout),
        @intFromEnum(OpCode.NIL) => return try simple_instruction("OP_NIL", offset, stdout),
        @intFromEnum(OpCode.TRUE) => return try simple_instruction("OP_TRUE", offset, stdout),
        @intFromEnum(OpCode.FALSE) => return try simple_instruction("OP_FALSE", offset, stdout),
        @intFromEnum(OpCode.POP) => return try simple_instruction("OP_POP", offset, stdout),
        @intFromEnum(OpCode.GET_LOCAL) => return try byte_instruction("OP_GET_LOCAL", chunk, offset, stdout),
        @intFromEnum(OpCode.SET_LOCAL) => return try byte_instruction("OP_SET_LOCAL", chunk, offset, stdout),
        @intFromEnum(OpCode.GET_GLOBAL) => return try constant_instruction("OP_GET_GLOBAL", chunk, offset, stdout),
        @intFromEnum(OpCode.DEFINE_GLOBAL) => return try constant_instruction("OP_DEFINE_GLOBAL", chunk, offset, stdout),
        @intFromEnum(OpCode.SET_GLOBAL) => return try constant_instruction("OP_SET_GLOBAL", chunk, offset, stdout),
        @intFromEnum(OpCode.GET_UPVALUE) => return try byte_instruction("OP_GET_UPVALUE", chunk, offset, stdout),
        @intFromEnum(OpCode.SET_UPVALUE) => return try byte_instruction("OP_SET_UPVALUE", chunk, offset, stdout),
        @intFromEnum(OpCode.EQUAL) => return try simple_instruction("OP_EQUAL", offset, stdout),
        @intFromEnum(OpCode.GREATER) => return try simple_instruction("OP_GREATER", offset, stdout),
        @intFromEnum(OpCode.LESS) => return try simple_instruction("OP_LESS", offset, stdout),
        @intFromEnum(OpCode.ADD) => return try simple_instruction("OP_ADD", offset, stdout),
        @intFromEnum(OpCode.SUBTRACT) => return try simple_instruction("OP_SUBTRACT", offset, stdout),
        @intFromEnum(OpCode.MULTIPLY) => return try simple_instruction("OP_MULTIPLY", offset, stdout),
        @intFromEnum(OpCode.DIVIDE) => return try simple_instruction("OP_DIVIDE", offset, stdout),
        @intFromEnum(OpCode.NOT) => return try simple_instruction("OP_NOT", offset, stdout),
        @intFromEnum(OpCode.NEGATE) => return try simple_instruction("OP_NEGATE", offset, stdout),
        @intFromEnum(OpCode.PRINT) => return try simple_instruction("OP_PRINT", offset, stdout),
        @intFromEnum(OpCode.JUMP) => return try jump_instruction("OP_JUMP", '+', chunk, offset, stdout),
        @intFromEnum(OpCode.JUMP_IF_FALSE) => return try jump_instruction("OP_JUMP_IF_FALSE", '+', chunk, offset, stdout),
        @intFromEnum(OpCode.LOOP) => return try jump_instruction("OP_LOOP", '-', chunk, offset, stdout),
        @intFromEnum(OpCode.CALL) => return try byte_instruction("OP_CALL", chunk, offset, stdout),
        @intFromEnum(OpCode.CLOSURE) => {
            var off = offset;

            off += 1;
            const constant = chunk.code.items[off];
            off += 1;

            try stdout.print("{s:<16} {d:4} ", .{ "OP_CLOSURE", constant });
            try chunk.constants.items[constant].print(stdout);
            try stdout.print("\n", .{});

            const function: *const Function = @ptrCast(chunk.constants.items[constant].object);
            for (0..function.upvalue_count) |_| {
                const is_local = chunk.code.items[off];
                off += 1;
                const index = chunk.code.items[off];
                off += 1;
                try stdout.print(
                    "{d:0>4}      |                     {s} {d}\n",
                    .{ off - 2, if (is_local == 1) "local" else "upvalue", index },
                );
            }

            return off;
        },
        @intFromEnum(OpCode.CLOSE_UPVALUE) => return try simple_instruction("OP_CLOSE_UPVALUE", offset, stdout),
        @intFromEnum(OpCode.RETURN) => return try simple_instruction("OP_RETURN", offset, stdout),
        else => {
            try stdout.print("Unknown instruction [{d}]\n", .{instruction});
            return offset + 1;
        },
    }
}

fn simple_instruction(name: []const u8, offset: usize, stdout: anytype) !usize {
    try stdout.print("{s}\n", .{name});
    return offset + 1;
}

fn byte_instruction(name: []const u8, chunk: *chunks.Chunk, offset: usize, stdout: anytype) !usize {
    const slot = chunk.code.items[offset + 1];

    try stdout.print("{s:<16} {d:4}\n", .{ name, slot });
    return offset + 2;
}

fn jump_instruction(name: []const u8, comptime sign: u8, chunk: *chunks.Chunk, offset: usize, stdout: anytype) !usize {
    const high: u16 = chunk.code.items[offset + 1];
    const low: u16 = chunk.code.items[offset + 2];
    const jump = (high << 8) + low;
    const dest = switch (sign) {
        '+' => offset + 3 + jump,
        '-' => offset + 3 - jump,
        else => @compileError("Unsupported sign"),
    };

    try stdout.print("{s:<16} {d:4} -> {d}\n", .{ name, offset, dest });
    return offset + 3;
}

fn constant_instruction(name: []const u8, chunk: *chunks.Chunk, offset: usize, stdout: anytype) !usize {
    const constant = chunk.code.items[offset + 1];

    try stdout.print("{s:<16} {d:4} '", .{ name, constant });
    try chunk.constants.items[constant].print(stdout);
    try stdout.print("'\n", .{});

    return offset + 2;
}
