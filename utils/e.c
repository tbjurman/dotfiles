#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void mk_daemon_name(char *buf, const char *dir)
{
  char c;
  size_t idx = 0;
  size_t len = strlen(dir) - 8;

  while (idx < len) {
    c = dir[idx++];
    *buf++ = c == '/' ? '_' : c;
  }
}

int main(int argc, char **argv)
{
  char daemon_name_arg[256];
  char *ncs_dir = getenv("NCS_DIR");
  char *args[argc + 5];
  int i, j = 0;

  if (ncs_dir) {
    mk_daemon_name(daemon_name_arg, ncs_dir);
  } else {
    strcpy(daemon_name_arg, "global");
  }

  args[j++] = "emacsclient";
  args[j++] = "-a \"\"";
  args[j++] = "-s";
  args[j++] = daemon_name_arg;
  args[j++] = "-t";
  for (i = 1; i < argc; i++) {
    args[j++] = argv[i];
  }
  args[j] = 0;

  if (execvp("emacsclient", args)) {
    perror(NULL);
    return 1;
  }
  return 0;
}
