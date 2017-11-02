#include <stdio.h> 
#include <string.h> 
#include <sys/time.h>

#define TRIALS 1000000L
#define FAST_TRIALS 1000L

unsigned long fibonacci(long fib);

int main(int argc, char*argv[]) {
    unsigned long retval = 0;
    long i = 0;
    struct timeval tf, ti;
    unsigned long timems = 0;

    if (argc != 2) {
        printf("Usage:\n\tEnter a number greater than 0!\n");
        return 1;
    }
  
    long fib = atoi(argv[1]);

    gettimeofday(&ti, NULL);
    for (i = 0; i < FAST_TRIALS; i++) {
        retval = fibonacci(fib);
    }
    gettimeofday(&tf, NULL);

    timems=(tf.tv_sec*1000+tf.tv_usec/1000) - (ti.tv_sec*1000+tf.tv_usec/1000);
    printf("Fibonacci(%d) = %d\n", fib, retval);
    printf("Iterations: %lu, TotalTime : %lu ms, IterTime : %lu us\n", i, timems, (1000*timems)/TRIALS);
  return 0;
}

