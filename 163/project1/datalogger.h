#ifndef DATALOGGER_H
#define DATALOGGER_H

#include <ostream>
#include "linkedlist.h"

class datalogger {
public:
//    datalogger();

//    ~datalogger();

    void addData(int, double, double);

    void printReport(void);

private:
    static const bool DEBUG = false;
    LinkedList weatherList;
};

#endif