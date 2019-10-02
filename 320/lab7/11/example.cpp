#include <iostream>

int main(int argc, char **argv) {
  int a = std::stoi(argv[1]);
  int b = a + 2000000000;
  if (a <= b)
    std::cout << a << " <= " << b << std::endl;
  else
    std::cout << a << " > " << b << std::endl;
}

  
