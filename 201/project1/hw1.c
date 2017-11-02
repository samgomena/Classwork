#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_RESET   "\x1b[0m"

FILE *open_file(const char *);

int *parse_coordinates(char *);

void init_maze(FILE *);

void read_maze(char **, int, int, FILE *);

void solve_maze(char **, int, int, int, int);

int right(int *, char **, int *, int *);

int forward(int *, char **, int *, int *);

int left(int *, char **, int *, int *);

void backward(int *, char **, int *, int *);

char **allocate_array(int, int);

void deallocate_maze(char **, int);

void print_maze(char **, int, int);

int main(int argc, char **argv) {
    if(argc == 2){
        FILE *file;
        
        printf("Loading maze from file: '%s'\n", argv[1]);
        file = open_file(argv[1]);
        
        init_maze(file);
        
    } else if(argc >= 2) {
        printf("ERROR: Too many arguments supplied.\n\tExpected one argument!");
    } else {
        printf("ERROR: No arguments supplied.\n\tExpected at least one!");
        exit(1);
    }
    return 0;
}

FILE *open_file(const char *filename) {
    FILE *file;
    file = fopen(filename, "r");
    if(file == NULL) {
        printf("ERROR: Failed to open file\n\tError number: %d", errno);
    }
    return file;
}

int *parse_coordinates(char *coords){
    const char DELIM[2] = ",";
    char *token;
    static int coords_array[2];
    int i, *var;
    
    i = 0;
    token = strtok(coords, DELIM);
    while(token != NULL) {
        sscanf(token, "%d", &var);
        coords_array[i] = var;
        token = strtok(NULL, DELIM);
        i++;
    }
    return coords_array;
}

void init_maze(FILE *file) {
    /* Our maze */
    char **array;
    /*Maximum length of string representation of long long i.e. 2^63 --> 19. Add one to compensate for n-1 in fgets(...).*/
    const int MAX_SIZE = 20;
    char *maze_buffer, *start_coords, *end_coords;
    int rows, columns, startX, startY, endX, endY;
    int *start_c, *end_c, *maze_size;
    
    maze_buffer = (char *)malloc(MAX_SIZE * sizeof(char));
    start_coords = (char *)malloc(MAX_SIZE * sizeof(char));
    end_coords = (char *)malloc(MAX_SIZE * sizeof(char));
    
    fgets(maze_buffer, MAX_SIZE, file);
    maze_size = parse_coordinates(maze_buffer);
    rows = maze_size[0];
    columns = maze_size[1];
    printf("Size of maze is %dx%d\n", rows, columns);
    
    fgets(start_coords, MAX_SIZE, file);
    start_c = parse_coordinates(start_coords);
    startX = start_c[0];
    startY = start_c[1];
    printf("Starting Coordinates: (%d, %d)\n", start_c[0], start_c[1]);
    
    fgets(end_coords, MAX_SIZE, file);
    end_c = parse_coordinates(end_coords);
    endX = end_c[0];
    endY = end_c[1];
    printf("End Coordinates: (%d, %d)\n", end_c[0], end_c[1]);
    
    free(maze_buffer);
    free(start_coords);
    free(end_coords);
    
    array = allocate_array(rows, columns);
    
    read_maze(array, columns, rows, file);
    print_maze(array, columns, rows);
    solve_maze(array, startX, startY, endX, endY);
    printf("\n-------------------- Solution --------------------\n");
    print_maze(array, columns, rows);
    deallocate_maze(array, columns);
}

char **allocate_array(int rows, int columns) {
    int i;
    char **array;
    
    array = (char**)malloc(columns * sizeof(char*));
    for(i = 0; i < columns; i++){
        array[i] = (char*)malloc(rows * sizeof(char));
    }
    
    return array;
}

void read_maze(char **array, int columns, int rows, FILE *file){
    int i, j;
    char c;
    for(i = 0; i < columns; i++){
        c = fgetc(file);
        for(j = 0; j < rows; j++) {
            if(c != '\n'){
                array[i][j] = c;
                c = fgetc(file);
            } else {
                c = fgetc(file);
            }
        }
    }
}

void solve_maze(char **array, int startX, int startY, int endX, int endY) {
    /* 0 == North, 1 == East, 2 == South, 3 == West. Didn't use an enum b/c the program is the only part that needs to know. Could change it easily however.*/
    int bear = 2;
    int *posX, *posY, *bearing;
    /* Coordinate system is backwards b/c I didn't have time to fix it. i.e. (x, y) -> (y, x).*/
    posX = &startY;
    posY = &startX;
    bearing = &bear;
    /* As it turns out we have to mark our first step.*/
    array[startY][startX] = 'W';
    do {
        if(right(bearing, array, posX, posY)) {
            array[*posX][*posY] = 'W';
        }
        else if (forward(bearing, array, posX, posY)) {
            array[*posX][*posY] = 'W';
        }
        else if(left(bearing, array, posX, posY)) {
            array[*posX][*posY] = 'W';
        } else {
            backward(bearing, array, posX, posY);
        }    
    } while(!(*posX == endY && *posY == endX));
    
}

int right(int *bearing, char **array, int *posX, int *posY) {
    if(*bearing == 0) {
        if(array[*posX][*posY+1] != 'X') {
            *posY+=1;
            *bearing = 1;
            return 1;
        }
    } else if (*bearing == 2) {
        if (array[*posX][(*posY)-1] != 'X') {
            *posY-=1;
            *bearing = 3;
            return 1;
        }
    } else if (*bearing == 1) {
        if(array[*posX+1][*posY] != 'X'){
            *posX+=1;
            *bearing = 2;
            return 1;
        }
    } else {
        if(array[*posX-1][*posY] != 'X') {
            *posX-=1;
            *bearing = 0;
            return 1;
        }
    }
    return 0;
}

int forward(int *bearing, char **array, int *posX, int *posY) {
    if(*bearing == 0) {
        if(array[*posX-1][*posY] != 'X') {
            *posX-=1;
            return 1;
        }
    } else if (*bearing == 2) {
        if(array[*posX+1][*posY] != 'X'){
            *posX+=1;
            return 1;
        }
    } else if (*bearing == 1) {
        if(array[*posX][*posY+1] != 'X') {
            *posY+=1;
            return 1;
        }
    } else {
        /* Bearing == 'W'*/
        if(array[*posX][*posY-1] != 'X') {
            *posY-=1;
            return 1;
        }
    }
    return 0;
}

int left(int *bearing, char **array, int *posX, int *posY) {
    if(*bearing == 0) {
        if(array[*posX][*posY-1] != 'X') {
            *posY-=1;
            *bearing = 3;
            return 1;
        }
    } else if (*bearing == 2) {
        if(array[*posX][(*posY)+1] != 'X') {
            *posY+=1;
            *bearing = 1;
            return 1;
        }
    } else if (*bearing == 1) {
        if(array[*posX-1][*posY] != 'X') {
            *posX-=1;
            *bearing = 0;
            return 1;
        }
    } else {
        /* Bearing == 'W'*/
        if(array[*posX+1][*posY] != 'X') {
            *posX+=1;
            *bearing = 2;
            return 1;
        }
    }
    return 0;
}

void backward(int *bearing, char **array, int *posX, int *posY) {
    if(*bearing == 0) {
        *bearing = 2;
    } else if (*bearing == 2) {
        *bearing = 0;
    } else if (*bearing == 1) {
        *bearing = 3;
    } else {
        /* Bearing == 'W'*/
        *bearing = 1;
    }
}

void print_maze(char **array, int columns, int rows) {
    const int SHOW_PATH = 0;
    int i, j;
    
    for(i = 0; i < columns; i++) {
        printf("\n");
        for(j = 0; j < rows; j++){
            /* This prints the maze without any of the X's. For debugging purposes for me. Also looks kinda cool.*/
            if(SHOW_PATH) {
                array[i][j] != 'X' && array[i][j] != '0' ? printf("%c", array[i][j]) : printf(" ");
            } else {
                array[i][j] == 'W' ? printf("%s%c%s", ANSI_COLOR_GREEN, array[i][j], ANSI_COLOR_RESET) : 
                array[i][j] == '0' ? printf("%s%c%s", ANSI_COLOR_RED, array[i][j], ANSI_COLOR_RESET) : printf("%c", array[i][j]);
            }
        }
    }
    printf("\n");
}

void deallocate_maze(char **array, int columns){
    int i;
    for(i = 0; i < columns; i++){
        free(array[i]);
    }
    free(array);
}