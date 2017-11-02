#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>

#define TRIALS 1000000L
#define FAST_TRIALS 1000000L

void ByteMult(unsigned long* a, unsigned long* b, unsigned long* result, long n);

long ReadMatrixSize(FILE* file)
{
	char buffer[10];
	if (fgets(buffer, 10, file) != NULL)
	{
		return strtol(buffer, NULL, 10);
	}

	return 0;
}

long ReadMatrixString(unsigned long* a, long count, FILE* file)
{
	char* buffer;
	char* next;
	long i=0; 

	buffer = malloc(count * 12);
	memset(a, 0, count*sizeof(unsigned long));
	fgets(buffer, count * 12, file);
	next = buffer;
	
	do
	{
			a[i] = (unsigned long) strtol(next, &next, 10);
			if (next == NULL) break;
			i++;
			next++;
	} while ( i < count);
	
	free(buffer);

	return 0;
}


long GetMatrices(unsigned long** a, unsigned long** b, unsigned long** result, char* filename)
{
	long n=0;
	FILE* file;

	file = fopen(filename, "r");
	if (file == NULL)
	{
		printf("Error Opening file\n");
		exit(1);
	}

	n = ReadMatrixSize(file);
	*a= (unsigned long*) malloc(n*n * sizeof(unsigned long));
	*b = (unsigned long*) malloc(n*n * sizeof(unsigned long));
	*result = (unsigned long*)malloc(n*n * sizeof(unsigned long));
	ReadMatrixString(*a, n*n, file);
	ReadMatrixString(*b, n*n, file);

	fclose(file);
	return n;
}

void Print2DMatrix(unsigned long* a, long n)
{
	long i, j;

	for (j = 0; j < n; j++)
	{
		for (i = 0; i < n; i++)
		{
			printf("%03lu ", a[j*n + i ]);
		}
		printf("\n");
	}	
	printf("\n");
}

int main(int argc, char*argv[])
{
  unsigned long *a, *b, *result;
  long iter=0;
  long matrixsize=0;
  struct timeval tf, ti;
  unsigned long timems=0;

  if (argc != 2)
  {
      printf("Usage: hw4 filename\n");
      return 1;
  }

  matrixsize=GetMatrices(&a, &b, &result, argv[1]);

  ByteMult(a, b, result, matrixsize);
  
  gettimeofday(&ti, NULL);
  for (iter = 0; iter < FAST_TRIALS; iter++)
  {   
      ByteMult(a, b, result, matrixsize);
  }
  gettimeofday(&tf, NULL);
  timems=(tf.tv_sec*1000+tf.tv_usec/1000) - (ti.tv_sec*1000+tf.tv_usec/1000);

  Print2DMatrix(result, matrixsize);
  printf("Iterations: %lu, TotalTime : %lu ms, IterTime : %lu us\n", iter, timems, (1000*timems)/TRIALS);
  
  free(a);
  free(b);
  free(result);
  return 0;
}
