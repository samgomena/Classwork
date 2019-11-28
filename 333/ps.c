#ifdef CS333_P2
#include "types.h"
#include "user.h"
#include "uproc.h"

int
main(int argc, char *argv[])
{
  const int max = 64;
  int numprocs;
  
  struct uproc *table = malloc(sizeof(struct uproc) * max);
  if (!table) {
    printf(2, "Error: malloc() call failed. %s at line %d\n", __FUNCTION__, __LINE__);
    exit();
  }
  numprocs = getprocs(max, table);
  if(numprocs < 0) {
    free(table);
    printf(2, "ERROR: Could not get process information: %d\n", numprocs);
    exit();
  }

  // Print header information
  // printf(1, "\nPID\tName         UID\tGID\tPPID\tElapsed\tCPU\tState\tSize\n");
  printf(1, "\nPID\tName\t\tUID\tGID\tPPID\tElapsed\tCPU\tState\tSize\n");
  for(int i=0; i < numprocs; i++) {
    int ms = table[i].elapsed_ticks;
    int sec = ((ms + 500) / 1000);
    int mantissa = ms % 1000;
    
    int cpu_sec = ((table[i].CPU_total_ticks + 500) / 1000);
    int cpu_mantissa = table[i].CPU_total_ticks % 1000;

    printf(1, "%d\t%s\t\t%d\t%d\t%d\t%d.%d\t%d.%d\t%s\t%d\t\n", table[i].pid, table[i].name, table[i].uid, table[i].gid, table[i].ppid, sec, mantissa, cpu_sec, cpu_mantissa, table[i].state, table[i].size);
  }

  free(table);
  exit();
}
#endif // CS333_P2
