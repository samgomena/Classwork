CC=g++
CPPFLAGS = -std=c++11 -I. -g
DEPS = plant.h planttree.h
OBJ = plant.o planttree.o experiments.o

%.o: %c $(DEPS)
	$(CC) $(CPPFLAGS) -c -o $@ 

experiments: $(OBJ)
	$(CC) $(CPPFLAGS) -o $@ $^
	valgrind --leak-check=yes ./$@

testtree: plant.o planttree.o testtree.o
	$(CC) $(CPPFLAGS) -o $@ $^
	valgrind --leak-check=yes ./$@

testplant: plant.o testplant.o
	$(CC) $(CPPFLAGS) -o $@ $^
	valgrind --leak-check=yes ./$@
