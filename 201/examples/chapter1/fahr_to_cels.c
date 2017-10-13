#include <stdio.h>

#define LOWER 0 // Lower limit of temperature
#define UPPER 300 // Upper limit of temperature
#define STEP 20 // Default step size to move

int main(int argc, char **argv) {
    float fahr, cels;

    printf("Fahrenheit\tCelsius\n");

    fahr = LOWER;
    while(fahr <= UPPER) {
        cels = 5.0  * (fahr - 32.0) / 9.0;
        printf("%3.0f\t%12.2f\n", fahr, cels);
        fahr += STEP;
    }
}

