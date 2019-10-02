#include <stdio.h>
#include <stdlib.h>

extern void Main_main();

void print(int x) {
    printf("print: %d\n",x);
}

int main() {
    Main_main();
    return 0;
}

