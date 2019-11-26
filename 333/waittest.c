#include "param.h"
#include "types.h"
#include "stat.h"
#include "user.h"
#include "fs.h"
#include "fcntl.h"
#include "syscall.h"
#include "traps.h"
#include "memlayout.h"
// #include "proc.h"

char buf[8192];
char name[3];
char *echoargv[] = { "echo", "ALL", "TESTS", "PASSED", 0 };
int stdout = 1;

void
waittest()
{
  // int pid;
  // uint i;

  // if((pid = fork()) == 0) {
  //   // printf(stdout, "counting to a billion in child\n");
  //   for(i = 0; i < 4000000000; ++i) {
  //     i++;
  //     if(i == 2000000000) {
  //       sleep(10);
  //     }
  //   }
  //   // printf(stdout, "finished counting to a billion in child\n");
  //   exit();
  // } else {

    // printf(stdout, "killing child process: %d\n", pid);
    // kill(pid);

    // printf(stdout, "parent process sleeping: %d\n", pid);
    // sleep(10);

    // printf(stdout, "waiting in parent process: %d\n", pid);
    wait();
  // }

}

int
main(int argc, char *argv[])
{

  printf(stdout, "starting waittest\n");
  waittest();
  printf(stdout, "waittest finished\n");

  exit();
}
