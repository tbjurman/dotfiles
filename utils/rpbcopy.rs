use std::io::{Read, Write, BufWriter, BufReader};
use std::net::{SocketAddrV4, Ipv4Addr, TcpListener};
use std::process::{Command, Stdio, exit};

fn write_to_clipboard(output: &Vec<u8>) {
    let copy = Command::new("pbcopy")
        .stdin(Stdio::piped())
        .spawn()
        .unwrap();
    let mut copy_stdin = copy.stdin.unwrap();
    let mut writer = BufWriter::new(&mut copy_stdin);
    writer.write_all(output).unwrap();
}

fn read_from_clipboard() -> Vec<u8> {
    let paste = Command::new("pbpaste")
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let mut paste_stdout = paste.stdout.unwrap();
    let mut reader = BufReader::new(&mut paste_stdout);
    let mut buf = Vec::new();
    reader.read_to_end(&mut buf).unwrap();
    buf
}

fn setup_listener(port: u16) -> TcpListener {
    let loopback = Ipv4Addr::new(127, 0, 0, 1);
    let socket = SocketAddrV4::new(loopback, port);
    TcpListener::bind(socket).unwrap()
}

fn is_paste(buf: &Vec<u8>) -> bool {
    let paste_pattern = "!@#999$%^";

    if buf.len() != paste_pattern.len() {
        return false;
    }
    if buf.as_slice() != paste_pattern.as_bytes() {
        return false;
    }
    true
}

fn accept_copy_paste_loop(listener: &TcpListener) -> () {
    loop {
        let (mut tcp_stream, _addr) = listener.accept().unwrap();
        let mut buf = Vec::new();
        // read to socket close?
        tcp_stream.read_to_end(&mut buf).unwrap();
        if is_paste(&buf) {
            // we're pasting
            let cbd = read_from_clipboard();
            tcp_stream.write_all(&cbd).unwrap();
        } else {
            // we're copying
            write_to_clipboard(&buf);
        }
    }
}

pub fn main() {
    if std::env::args().len() != 2 {
        println!("Usage: {} port", std::env::args().nth(0).unwrap());
        exit(1);
    }
    let port = std::env::args().nth(1).unwrap();
    let port = port.parse::<u16>().unwrap();
    let listener = setup_listener(port);
    accept_copy_paste_loop(&listener);
}
