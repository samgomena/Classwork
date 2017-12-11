#ifndef ROVER_H
#define ROVER_H

#include "scandata.h"
#include "stack.h"
#include "queue.h"

class rover {
private:
    int _id;
    int _max_results;
    scandata scandata1;
    Stack stack;
    Queue queue;

public:
    rover(int, int);
//    ~rover();
    void deploy();
    void move(int, int);
    void corescan();
    void dock();
    void whoami();
};
#endif