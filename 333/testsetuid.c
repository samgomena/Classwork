#ifdef CS333_P2
#include "types.h"
#include "user.h"

void
test_set_uid(int nval)
{
  uint uid = setuid(nval);

  uid = getuid();
  if (uid != nval && (uid > 0))
    printf(2, "FAILED: expected %d but got %d\n", nval, uid);
  else
    printf(1, "SUCCESS\n");

  return;
}

int
main(int argc, char *argv[])
{
  // This test should pass
  test_set_uid(10);

  // Theses tests should fail
  test_set_uid(-10);
  test_set_uid(32767);
  test_set_uid(32768);

  printf(1, "***** In %s: my uid is %d\n\n", argv[0], getuid());
  exit();
}
#endif
