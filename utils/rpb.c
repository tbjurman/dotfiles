#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>

#define IP "127.0.0.1"
#define PORT 10001
#define STRPORT "10001"
#define BUF_SIZE 1024
#define PASTE_PATTERN "!@#999$%^"
#define PASTE_PATTERN_SIZE 9

void ok_or_die(int res, const char *msg)
{
  if (res < 0) {
    perror(msg);
    exit(1);
  }
}

void turn_off_nagle(int sock)
{
  int flag = 1;
  int res = setsockopt(sock, IPPROTO_TCP, TCP_NODELAY,
                       (char *)&flag, sizeof(flag));
  ok_or_die(res, "failed to set TCP_NODELAY");
}

int create_socket(void)
{
  int sock = socket(AF_INET, SOCK_STREAM, 0);
  ok_or_die(sock, "failed to create socket");
  return sock;
}

void send_all(int sock, const char *buf, size_t count)
{
  size_t sent = 0;
  ssize_t stmp;

  while (sent < count) {
    stmp = write(sock, buf + sent, count);
    ok_or_die(stmp, "failed to send data to server");
    sent += stmp;
    count -= stmp;
  }
}

int connect_server()
{
  int sock;
  int res;
  struct sockaddr_in addr;

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr("127.0.0.1");
  addr.sin_port = htons(PORT);

  sock = create_socket();
  turn_off_nagle(sock);
  res = connect(sock, (struct sockaddr*)&addr, sizeof(addr));
  ok_or_die(res, "failed to connect to " IP ":" STRPORT);

  return sock;
}

void send_stdin(int sock)
{
  size_t bytes_read;
  char buf[BUF_SIZE];

  while (!feof(stdin)) {
    bytes_read = fread(&buf, 1, BUF_SIZE, stdin);
    if (bytes_read > 0) {
      send_all(sock, buf, bytes_read);
    }
  }
}

void read_all(int sock)
{
  size_t bytes_read;
  char buf[BUF_SIZE];

  send_all(sock, PASTE_PATTERN, PASTE_PATTERN_SIZE);

  while ((bytes_read = read(sock, buf, BUF_SIZE)) > 0) {
      fwrite(buf, bytes_read, 1, stdout);
  }
}

int main(int argc, char **argv)
{
  int sock = connect_server();

  if (argc > 1 && *argv[1] == 'p') {
    read_all(sock);
  } else {
    send_stdin(sock);
  }
  close(sock);
  return 0;
}
