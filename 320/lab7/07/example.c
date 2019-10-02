#include <stdlib.h>
#include <stdio.h>

struct point {
  int x;
  int y;
};

typedef struct link *Link;
struct link {
  struct point p;
  Link   n;
};

Link newLink(struct point p, Link n) {
  Link link = malloc(sizeof(*link));
  link->p = p;
  link->n = n;
  return link;
}

int f(int n, Link list) {
  int sum = 0;
  for (int i = 0; i < n; i++) {
    sum += list->p.x;
    list = list->n;
  }
  return sum;
}

int main(int argc, char **argv) {
  int a = strtol(argv[1],NULL,10);
  int b = strtol(argv[2],NULL,10);
  Link list = NULL;
  for (int i = 0; i < a; i++) 
    list = newLink((struct point) {.x=i,.y=i},list);
  int s = f(b,list);
  printf("sum = %d\n",s);
}
  
