#include <iostream>

static int f(int a) {
  int b;
  int c;
  if (a < 100)
    c = 10;
  else
    c = 20;
  return a + b + c;
}

int main(int argc, char **argv) {
  int a = std::stoi(argv[1]);
  int d = f(a);
  std::cout << "a = " << a << " d = " << d << std::endl;
}
