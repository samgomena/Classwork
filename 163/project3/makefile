CC=g++
CPPFLAGS = -std=c++11 -I. -g
DEPS = family.h hashtable.h node.h
OBJ = family.o node.o hashtable.o housinghelper.o familymgr.o

%.o: %c $(DEPS)
	$(CC) $(CPPFLAGS) -c -o $@ 

housinghelper: $(OBJ)
	$(CC) $(CPPFLAGS) -o $@ $^

testfamily: family.o testfamily.o
	$(CC) $(CPPFLAGS) -o $@ $^
	valgrind --leak-check=yes ./$@

testlinkedlist: family.o node.o testlinkedlist.o
	$(CC) $(CPPFLAGS) -o $@ $^
	valgrind --leak-check=yes ./$@

testhashtable: family.o hashtable.o node.o testhashtable.o
	$(CC) $(CPPFLAGS) -o $@ $^
	valgrind --leak-check=yes ./$@

generate: generate.o family.o
	$(CC) $(CPPFLAGS) -o $@ $^

