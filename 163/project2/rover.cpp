
#include "rover.h"
#include <iostream>

using namespace std;

rover::rover(int id, int MAX_RESULTS) : _id(id), _max_results(MAX_RESULTS) {
    stack.push(0, 0);
}

void rover::whoami() {
    cout << "Rover (ID " << _id << ") ";
}

void rover::deploy() {
    whoami();
    cout << "deploying..." << endl;
    whoami();
    cout << "ready." << endl;
}

void rover::move(int x, int y) {
    stack.push(x, y);
    whoami();
    cout << "moving to location " << x << ", " << y << "." << endl;
}

void rover::corescan() {
    whoami();
    cout << "scanning." << endl;
    queue.enqueue(scandata1.getScandata(stack.peekX(), stack.peekY()));
}

void rover::dock() {
    stack.pop();
    whoami();
    cout << "returning to base." << endl;
    while(!stack.isEmpty()){
        whoami();
        cout << "moving to location " << stack.peekX() << ", " << stack.peekY() << "." << endl;
        stack.pop();
    }
//    whoami();
//    cout << "moving to location 0, 0." << endl;
    whoami();
    cout << "at base. Sending results..." << endl;
    while(!queue.isEmpty()) {
        whoami(); cout << "result: " << queue.peek() << endl;
        queue.dequeue();
    }
    stack.push(0, 0);
    whoami();
    cout << "result transmission complete." << endl;
    whoami();
    cout << "docked." << endl;
}
