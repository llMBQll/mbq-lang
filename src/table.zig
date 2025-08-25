const std = @import("std");

const debug = @import("debug.zig");
const chunks = @import("chunks.zig");
const lexer_mod = @import("lexer.zig");
const objects = @import("objects.zig");
const values = @import("values.zig");
const vm_mod = @import("vm.zig");

const Allocator = std.mem.Allocator;
const Chunk = chunks.Chunk;
const Lexer = lexer_mod.Lexer;
const OpCode = chunks.OpCode;
const String = objects.String;
const Token = lexer_mod.Token;
const TokenType = lexer_mod.TokenType;
const Value = values.Value;
const VM = vm_mod.VM;

pub const Table = struct {
    const Self = @This();

    const MAX_LOAD = 0.75;

    entries: std.ArrayList(Entry),
    count: usize,

    pub fn init() Self {
        return .{
            .entries = std.ArrayList(Entry).empty,
            .count = 0,
        };
    }

    pub fn deinit(self: *Self, allocator: Allocator) void {
        self.entries.deinit(allocator);
    }

    fn find_entry(entries: *std.ArrayList(Entry), key: *String) *Entry {
        var index = key.hash % entries.capacity;
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries.items[index];

            if (entry.key == null) {
                if (entry.value == .nil) {
                    // Found empty entry
                    return if (tombstone) |t| t else entry;
                } else {
                    // Found tombstone
                    if (tombstone == null) {
                        tombstone = entry;
                    }
                }
            } else if (entry.key == key) {
                // Found key
                return entry;
            }

            index = (index + 1) % entries.capacity;
        }
    }

    fn adjust_capacity(self: *Self, allocator: Allocator, capacity: usize) !void {
        var new_entries = try std.ArrayList(Entry).initCapacity(allocator, capacity);
        for (0..capacity) |_| {
            new_entries.appendAssumeCapacity(.{
                .key = null,
                .value = .nil,
            });
        }

        var count: usize = 0;
        for (0..self.entries.capacity) |i| {
            const entry = &self.entries.items[i];

            if (entry.key) |key| {
                const dest = find_entry(&new_entries, key);
                dest.* = entry.*;
                count += 1;
            } else {
                continue;
            }
        }

        self.entries.deinit(allocator);
        self.entries = new_entries;
        self.count = count;
    }

    pub fn set(self: *Self, allocator: Allocator, key: *String, value: Value) !bool {
        const load: f64 = @floatFromInt(self.entries.items.len + 1);
        const capacity: f64 = @floatFromInt(self.entries.capacity);

        if (load > capacity * Self.MAX_LOAD) {
            const old_capacity = self.entries.capacity;
            const new_capacity = if (old_capacity < 8) 8 else old_capacity * 2;
            try self.adjust_capacity(allocator, new_capacity);
        }

        const entry = find_entry(&self.entries, key);

        const is_new = entry.key == null;
        if (is_new) {
            self.count += 1;
        }

        entry.key = key;
        entry.value = value;

        return is_new;
    }

    pub fn get(self: *Self, key: *String) ?Value {
        if (self.count == 0) {
            return null;
        }

        const entry = find_entry(&self.entries, key);
        if (entry.key == null) {
            return null;
        }

        return entry.value;
    }

    pub fn delete(self: *Self, key: *String) bool {
        if (self.count == 0) {
            return false;
        }

        const entry = find_entry(&self.entries, key);
        if (entry.key == null) {
            return false;
        }

        // Place a tombstone
        entry.key = null;
        entry.value = .{ .bool = true };

        return true;
    }

    fn add_all(self: *Self, other: *Self) !void {
        for (0..other.entries.capacity) |i| {
            const entry = &other.entries.items[i];
            if (entry.key != null) {
                self.set(entry.key.?, entry.value);
            }
        }
    }

    pub fn find_string(self: *Self, chars: []const u8, hash: u32) ?*String {
        if (self.count == 0) {
            return null;
        }

        var index = hash % self.entries.capacity;
        while (true) {
            const entry = &self.entries.items[index];
            if (entry.key) |key| {
                if (key.hash == hash and std.mem.eql(u8, key.chars, chars)) {
                    return key;
                }
            } else if (entry.value == .nil) {
                return null;
            }

            index = (index + 1) % self.entries.capacity;
        }
    }
};

const Entry = struct {
    const Self = @This();

    key: ?*String,
    value: Value,
};
