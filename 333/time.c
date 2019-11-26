#ifdef CS333_P2
#include "types.h"
#include "user.h"

int
main(int argc, char *argv[])
{
  int ut = -1;
  char *prog, **args;
  prog = argv[1];
  args = argv++;
  ut = uptime();
  
  int pid = fork();
  if(pid < 0) {
    printf(2,"Error: failed to fork. %s at line %d\n",
    __FILE__, __LINE__);
    exit();
  } else if (pid == 0) {
    exec(prog, ++args);
    printf(2,"Error: cannot run %s.\n", prog);
    exit();
  } else {
    pid = wait();
  }

  int ms = uptime() - ut;
  int sec = ((ms + 500) / 1000);
  int mantissa = ms % 1000;

  printf(1, "%s ran in %d.%d seconds.\n", prog, sec, mantissa);
  exit();
}
#endif
