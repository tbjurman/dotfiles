const std = @import("std");
const Stream = std.net.Stream;

const print = std.debug.print;

const IP = "127.0.0.1";
const PORT: u16 = 10001;
const BUFSIZE = 8192;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();
    const addr = try std.net.Address.resolveIp(IP, PORT);
    var listener = try addr.listen(.{});

    while (true) {
        if (listener.accept()) |conn| {
            defer conn.stream.close();
            try handle_connection(conn.stream, allocator);
        } else |_| {}
    }
}

pub fn handle_connection(stream: Stream, allocator: std.mem.Allocator) !void {
    var buf: [BUFSIZE]u8 = undefined;
    const size = try stream.read(&buf);
    if (is_paste(buf[0..])) {
        // we are pasting
        try read_from_clipboard(stream, allocator);
    } else {
        // we are copying
        try write_to_clipboard(stream, &buf, size, allocator);
    }
}

fn is_paste(buf: []const u8) bool {
    const paste_pattern = "!@#999$%^";
    const min_len = @min(paste_pattern.len, buf.len);

    if (std.mem.eql(u8, buf[0..min_len], paste_pattern)) {
        return true;
    }
    return false;
}

fn read_from_clipboard(stream: Stream, allocator: std.mem.Allocator) !void {
    const res = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &[_][]const u8{"pbpaste"},
    });
    defer allocator.free(res.stdout);
    defer allocator.free(res.stderr);

    try stream.writeAll(res.stdout);
    try std.posix.shutdown(stream.handle, std.posix.ShutdownHow.send);
}

fn write_to_clipboard(
    stream: Stream,
    buf: []u8,
    size0: usize,
    allocator: std.mem.Allocator,
) !void {
    const argv: []const []const u8 = &[_][]const u8{"pbcopy"};

    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Pipe;
    try child.spawn();

    var size = size0;
    while (size > 0) {
        try child.stdin.?.writeAll(buf[0..size]);
        size = try stream.read(buf);
    }
    child.stdin.?.close();
    child.stdin = null;
    _ = try child.wait();
}
