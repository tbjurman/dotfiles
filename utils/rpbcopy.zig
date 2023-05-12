const std = @import("std");
const Stream = std.net.Stream;

const print = std.debug.print;

const IP = "127.0.0.1";
const PORT: u16 = 10001;
const BUFSIZE = 8192;

pub fn main() !void {
    // const port = parsePortArg();

    var listener = std.net.StreamServer.init(.{
        .reuse_address = true,
    });
    const listen_address = try std.net.Address.resolveIp(IP, PORT);
    try listener.listen(listen_address);

    while (true) {
        if (listener.accept()) |conn| {
            defer conn.stream.close();
            try handle_connection(conn.stream);
        } else |_| {}
    }
}

pub fn handle_connection(stream: Stream) !void {
    var buf: [BUFSIZE]u8 = undefined;
    var size = try stream.readAll(&buf);
    if (is_paste(buf[0..])) {
        try read_from_clipboard(stream); // we are pasting
    } else {
        try write_to_clipboard(stream, &buf, size); // we are copying
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

fn read_from_clipboard(stream: Stream) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();
    const exec_args = .{
        .allocator = allocator,
        .argv = &[_][]const u8{"pbpaste"},
    };

    if (std.process.Child.exec(exec_args)) |res| {
        defer allocator.free(res.stdout);
        defer allocator.free(res.stderr);
        if (res.stdout.len > 0) {
            try stream.writeAll(res.stdout);
        }
    } else |err| switch (err) {
        error.StdoutStreamTooLong => {
            try stream.writeAll("error: too much data in clipboard");
        },
        else => return err,
    }
    try std.os.shutdown(stream.handle, std.os.ShutdownHow.send);
}

fn write_to_clipboard(stream: Stream, buf: []u8, size0: usize) !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    var allocator = arena.allocator();
    const argv: []const []const u8 = &[_][]const u8{"pbcopy"};

    var child = std.process.Child.init(argv, allocator);
    child.stdin_behavior = .Pipe;
    try child.spawn();

    var size = size0;
    while (size > 0) {
        try child.stdin.?.writeAll(buf[0..size]);
        size = try stream.readAll(buf);
    }
    child.stdin.?.close();
    child.stdin = null;
    _ = try child.wait();
}

// fn parsePortArg() {
//     var args_iter = std.process.args();
//     var port: u16 = undefined;

//     if (args_iter.next()) |x| {
//         print("{s}\n", .{x});
//         if (args_iter.next()) |strport| {
//             print("{s}\n", .{strport});
//             port = try std.fmt.parseInt(u16, strport, 10);
//         }
//     }
//     if (port == undefined) {
//         std.debug.print("Usage: rpbcopy PORT\n", .{});
//         std.process.exit(1);
//     }
// }
