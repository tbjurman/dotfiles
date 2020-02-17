#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <fcntl.h>

#define IP "127.0.0.1"
#define PORT 10001
#define STRPORT "10001"
#define BUF_SIZE 1024
#define PASTE_PATTERN "!@#999$%^"
#define PASTE_PATTERN_SIZE 9

int do_or_die(int res, const char *msg)
{
  if (res < 0) {
    perror(msg);
    exit(1);
  }
  return res;
}

void stderr_to_dev_null()
{
  int fd = do_or_die(open("/dev/null", O_WRONLY),
                     "failed to open /dev/null");
  do_or_die(dup2(STDERR_FILENO, fd), "failed to dup2 stderr");
}

/* Could improve performance for small packages.
   Probably won't affect localhost at all.
*/
void turn_off_nagle(int sock)
{
  int flag = 1;
  setsockopt(sock, IPPROTO_TCP, TCP_NODELAY,
             (char *)&flag, sizeof(flag));
}

int connect_server()
{
  int sock;
  struct sockaddr_in addr;

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = inet_addr("127.0.0.1");
  addr.sin_port = htons(PORT);

  stderr_to_dev_null();
  sock = do_or_die(socket(AF_INET, SOCK_STREAM, 0), "failed to create socket");
  turn_off_nagle(sock);
  do_or_die(connect(sock, (struct sockaddr*)&addr, sizeof(addr)),
            "failed to connect to " IP ":" STRPORT);
  return sock;
}

void send_all(int sock, const char *buf, size_t count)
{
  size_t sent = 0;
  ssize_t stmp;

  while (sent < count) {
    stmp = do_or_die(write(sock, buf + sent, count),
                     "failed to send data to server");
    sent += stmp;
    count -= stmp;
  }
}

void stdin_to_remote_pb(int sock)
{
  size_t brd;
  char buf[BUF_SIZE];

  while ((brd = read(STDIN_FILENO, &buf, BUF_SIZE)) > 0) {
    send_all(sock, buf, brd);
  }
}

void remote_pb_to_stdout(int sock)
{
  size_t brd;
  size_t bwr;
  size_t bwr_tot;
  char buf[BUF_SIZE];

  send_all(sock, PASTE_PATTERN, PASTE_PATTERN_SIZE);

  while ((brd = read(sock, buf, BUF_SIZE)) > 0) {
    bwr_tot = 0;
    while ((bwr = write(STDOUT_FILENO, buf + bwr_tot, brd - bwr_tot)) > 0 &&
           bwr_tot < brd) {
      bwr_tot += bwr;
    }
  }
}

int main(int argc, char **argv)
{
  int sock = connect_server();

  if (argc > 1 && *argv[1] == 'p') {
    remote_pb_to_stdout(sock);
  } else {
    stdin_to_remote_pb(sock);
  }
  close(sock);
  return 0;
}
