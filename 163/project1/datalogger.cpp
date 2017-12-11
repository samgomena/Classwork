#include <iostream>
#include "datalogger.h"

using namespace std;

void datalogger::printReport() {
    weatherList.printSmallReport();
//    weatherList.generateReport();
//    weatherList.prettyPrint();
};

void datalogger::addData(int time, double temp, double wind) {
    WeatherData * wd = new WeatherData(time, temp, wind);
    Node * new_node = new Node(wd);
    if(!weatherList.addSorted(new_node)) {
        delete new_node;
        new_node = nullptr;
    }
}
