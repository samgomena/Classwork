all:
	g++ -g -Wall -std=c++98 *.cpp *.h -o app

test:
	valgrind --tool=memcheck --leak-check=full --show-leak-kinds=all ./app

backup:
	rsync -uvr ./*  ../project3_backup/
