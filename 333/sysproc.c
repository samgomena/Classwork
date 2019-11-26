#include "types.h"
#include "x86.h"
#include "defs.h"
#include "date.h"
#include "param.h"
#include "memlayout.h"
#include "mmu.h"
#include "proc.h"
#ifdef PDX_XV6
#include "pdx-kernel.h"
#endif // PDX_XV6

#ifdef CS333_P2
// #include "spinlock.h"
#include "uproc.h"
#endif // CS333_P2

int
sys_fork(void)
{
  return fork();
}

int
sys_exit(void)
{
  exit();
  return 0;  // not reached
}

int
sys_wait(void)
{
  return wait();
}

int
sys_kill(void)
{
  int pid;

  if(argint(0, &pid) < 0)
    return -1;
  return kill(pid);
}

int
sys_getpid(void)
{
  return myproc()->pid;
}

int
sys_sbrk(void)
{
  int addr;
  int n;

  if(argint(0, &n) < 0)
    return -1;
  addr = myproc()->sz;
  if(growproc(n) < 0)
    return -1;
  return addr;
}

int
sys_sleep(void)
{
  int n;
  uint ticks0;

  if(argint(0, &n) < 0)
    return -1;
  ticks0 = ticks;
  while(ticks - ticks0 < n){
    if(myproc()->killed){
      return -1;
    }
    sleep(&ticks, (struct spinlock *)0);
  }
  return 0;
}

// return how many clock tick interrupts have occurred
// since start.
int
sys_uptime(void)
{
  uint xticks;

  xticks = ticks;
  return xticks;
}

#ifdef PDX_XV6
// shutdown QEMU
int
sys_halt(void)
{
  do_shutdown();  // never returns
  return 0;
}
#endif // PDX_XV6

#ifdef CS333_P1
int
sys_date(void)
{
  struct rtcdate *d;

  if(argptr(0, (void*)&d, sizeof(struct rtcdate)) < 0) {
    return -1;
  }
  cmostime(d);
  return 0;
}
#endif // CS333_P1

#ifdef CS333_P2
int
sys_getuid(void)
{
  return myproc()->uid;
}

int
sys_getgid(void)
{
  return myproc()->gid;
}

int
sys_getppid(void)
{
  int ppid;
  if(myproc()->parent)
    ppid = myproc()->parent->pid;
  else
    ppid = myproc()->pid;
  
  return ppid;
}

int
sys_setuid(void)
{
  int newuid;
  if(argint(0, &newuid) < 0)
    return -1;

  // uid must be 0 < newid <= 32767
  if(newuid > 32767 || newuid < 0)
    return -1;

  myproc()->uid = newuid;
  return 0;
}

int
sys_setgid(void)
{
  int newgid;
  if(argint(0, &newgid) < 0)
    return -1;
  
  // gid must be 0 < newid <= 32767
  if(newgid > 32767 || newgid < 0)
    return -1;

  myproc()->gid = newgid;
  return 0;
}

int
sys_getprocs(void)
{
  int max, procsret = 0;
  struct uproc *uprocs;

  if(argint(0, &max) < 0)
    return -1;

  if(argptr(1, (void*)&uprocs, sizeof(struct uproc) * max) < 0) {
    return -1;
  }

  if(getprocs(max))
    return -1;
  
  // for(int i=0;i < max; i++) {
  //   // if(ptable[i]->state)
  //   // uprocs[i]->pid = ptable[i]->pid;
  //   // uprocs[i]->uid = ptable[i]->uid;
  //   // uprocs[i]->gid = ptable[i]->gid;
  //   // uprocs[i]->ppid = ptable[i]->ppid;
  //   // uprocs[i]->elapsed_ticks = ptable[i]->elapsed_ticks;
  //   // uprocs[i]->CPU_total_ticks = ptable[i]->cpu_total_ticks;
  //   // uprocs[i]->pid = ptable[i]->pid;
  //   // uprocs[i]->size = ptable[i]->size;
  //   // uprocs[i]->pid = ptable[i]->pid;
  //   uprocs[i] = ptable[i];
  //   procsret++;
  // }

  
  cprintf("%d, %d, %d\n\n", max, sizeof(uprocs), sizeof(struct uproc) * max);
  return procsret;
}
#endif // CS333_P2
