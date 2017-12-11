CPPFLAGS = -Wall -c -g -std=c++11

CPPCOMPFLAGS = -Wall -g -std=c++11

WeatherApp: weatherlog.o datalogger.o linkedlist.o weatherdata.o
	g++ $(CPPCOMPFLAGS) weatherlog.o datalogger.o linkedlist.o weatherdata.o -o WeatherApp

weatherlog.o: weatherlog.cpp datalogger.o
	g++ $(CPPFLAGS)  weatherlog.cpp

datalogger.o: datalogger.cpp linkedlist.o
	g++ $(CPPFLAGS) datalogger.cpp

linkedlist.o: linkedlist.cpp
	g++ $(CPPFLAGS) linkedlist.cpp

weatherdata.o: weatherdata.cpp
	g++ $(CPPFLAGS) weatherdata.cpp

.PHONY: clean
clean:
	$(info -- cleaning the directory -- )
	rm -f *.o
	rm -f app

test:
	valgrind --tool=memcheck --leak-check=full ./WeatherApp climatedata.txt
