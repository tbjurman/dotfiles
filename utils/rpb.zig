const std = @import("std");
const Stream = std.net.Stream;
const IP = "127.0.0.1";
const PORT: u16 = 10001;
const BUF_SIZE = 1024;
const PASTE_PATTERN = "!@#999$%^";

const Mode = enum {
    paste,
    copy,
};

pub fn main() !void {
    var args_iter = std.process.args();
    var mode = Mode.copy;
    var conn: Stream = undefined;

    while (args_iter.next()) |arg| {
        if (std.mem.eql(u8, arg, "p")) {
            mode = Mode.paste;
            break;
        }
    }

    if (connect_server()) |c| {
        conn = c;
    } else |err| switch (err) {
        error.ConnectionRefused => {
            print_stderr("Unable to connect to {s}:{d}\n", .{ IP, PORT });
            std.process.exit(1);
        },
        else => return err,
    }
    defer conn.close();

    if (mode == Mode.copy) {
        try stdin_to_remote_pb(conn);
    } else {
        try remote_pb_to_stdout(conn);
    }
}

fn connect_server() !Stream {
    const addr = try std.net.Address.resolveIp(IP, PORT);
    return try std.net.tcpConnectToAddress(addr);
}

fn stdin_to_remote_pb(conn: Stream) !void {
    const stdin = std.io.getStdIn().reader();
    var buf: [BUF_SIZE]u8 = undefined;
    while (true) {
        var index = try stdin.readAll(buf[0..]);
        if (index == 0) {
            return;
        }
        try conn.writeAll(buf[0..index]);
    }
}

fn remote_pb_to_stdout(conn: Stream) !void {
    var buf: [BUF_SIZE]u8 = undefined;

    try conn.writeAll(PASTE_PATTERN);
    try std.os.shutdown(conn.handle, std.os.ShutdownHow.send);
    while (true) {
        var index = try conn.readAll(buf[0..]);
        if (index == 0) {
            return;
        }
        print_stdout("{s}", .{buf[0..index]});
    }
}

fn print_stdout(comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdOut().writer();
    writer.print(fmt, args) catch return;
}

fn print_stderr(comptime fmt: []const u8, args: anytype) void {
    const writer = std.io.getStdErr().writer();
    writer.print(fmt, args) catch return;
}
