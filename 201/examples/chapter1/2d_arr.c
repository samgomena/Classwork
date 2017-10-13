#include <stdio.h>
#include <stdlib.h>

void print_arr(int **array, int rows, int cols);

int main(int argc, char **argv) {
    int **array;
    int i, j, rows, cols;
    
    rows = 5;
    cols = 5;
        
    array = (int**) malloc(rows * sizeof(int*));

    for(i = 0; i < rows; ++i){
        array[i] = (int*)malloc(cols * sizeof(int));
    }

    for(i = 0; i < rows; ++i) {
        for(j = 0; j < cols; j++) {
            array[i][j] = i * j; 
        }
    }

    //print_arr(array, rows, cols, i, j);

    for(i = 0; i < rows; ++i){
        free(array[i]);
    }
    free(array);
}

void print_arr(int **array, int rows, int cols) {
    int i, j;
    //printf("["); 
    for(i = 0; i < rows; i++) {
        i == 0 ? printf("[") : printf("\n");
        for(j = 0; j < cols; j++) {
          j == cols - 1 ? printf("%d]", array[i][j]) : printf("%d, ", array[i][j]);
        }
    }
}
