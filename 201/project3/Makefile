.PHONY: clean
CFLAGS= -ansi -pedantic -O0 -Wall -fgcse
CFLAGSminus= -ansi -pedantic -O0 -Wall

all: hw48 hw44 hw4

#################### HW48 ####################
hw48: hw4.o mult8.o
	gcc -g -o hw48 $(CFLAGS) hw4.o mult8.o

hw4.o: hw4.c
	gcc -c -g -o hw4.o $(CFLAGS) hw4.c

mult8.o: mult8.c
	gcc -c -g -o mult8.o $(CFLAGSminus) mult8.c
	
#################### HW44 ####################
	
hw44: hw4.o mult4.o
	gcc -g -o hw44 $(CFLAGS) hw4.o mult4.o

mult4.o: mult4.c
	gcc -c -g -o mult4.o $(CFLAGSminus) mult4.c

################## Baseline ##################

hw4: hw4.o mult.o
	gcc -g -o hw4 $(CFLAGS) hw4.o mult.o

mult.o: mult.c
	gcc -c -g -o mult.o $(CFLAGS) mult.c

	
clean:
	rm -f *.o hw4 hw44 hw48
