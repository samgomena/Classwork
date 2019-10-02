#include <iostream>

int f(int a,int b) {
  return a*b/b;
}


int main(int argc, char **argv) {
  int a = std::stoi(argv[1]);
  int b = std::stoi(argv[2]);
  int c = f(a,b);
  std::cout << "a = " << a << " b = " << b << " c = " << c << std::endl;
 }
 
