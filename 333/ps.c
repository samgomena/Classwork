#ifdef CS333_P2
#include "types.h"
#include "user.h"
#include "uproc.h"

int
main(int argc, char *argv[])
{
  int numprocs, max = 64;
  struct uproc *table;
  table = malloc(sizeof(struct uproc) * max);
  if (!table) {
    printf(2, "Error: malloc() call failed. %s at line %d\n", __FUNCTION__, __LINE__);
    exit();
  }
  numprocs = getprocs(42, table);
  if(numprocs < 0) {
    printf(2, "ERROR: Could not get process information: %d\n", numprocs);
    return -1;
  }

  for(int i=0; i < numprocs; i++)
    printf(1, "%d ", i);

  free(table);
  exit();
}
#endif // CS333_P2
