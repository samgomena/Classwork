#include <iostream>
#include "linkedlist.h"

using namespace std;

// Helper functions
double fToC(double);
double mphToMps(double);

Node::Node() : wd(nullptr), nextTime(nullptr), nextTemp(nullptr), nextWind(nullptr), remove(false) {}

Node::Node(WeatherData *wd) : wd(wd), nextTime(nullptr), nextTemp(nullptr), nextWind(nullptr), remove(false) {}

Node::~Node() {
    if(wd) {
        delete wd;
    }
}

Node::Node(const Node & copy_me) : nextTime(nullptr), nextTemp(nullptr), nextWind(nullptr), wd(copy_me.wd), remove(copy_me.remove){}

void Node::print() {
    cout << "Time: " << wd->getTime() << "\nTemp: " << wd->getTemp() << "\nWind: " << wd->getWind() << endl;
}

LinkedList::LinkedList() : size(0), head(new Node()), tail(new Node()) {};

LinkedList::~LinkedList() {
    if(head) {
        destroy(head);
    }
}

LinkedList::LinkedList(const LinkedList & copy_me) : size(copy_me.size), head(nullptr), tail(nullptr) {
    copy(head, copy_me.head);
}


void LinkedList::generateReport() const {
    // TODO: This needs some work. It's not in accordance with `expected.txt` but the expected text file also generates
    // TODO: percents and conversions improperly so need to determine how to fix that with teacher.
    cout << "\t\t\t\t\t\t\t\t-- Data Report --\t\t\t\t\t\t\t\t" << endl << endl;
    cout << "Time Range:\t\t\t" << head->nextTime->wd->getTime() << " - " << tail->nextTime->wd->getTime() << endl;
    cout << "Number of entries:\t\t\t" << size << endl;
    cout << "----------------------------------------------------------------------" << endl;
    cout << "TEMPERATURE" << endl;
    cout << "Min temperature: " << head->nextTemp->wd->getTemp() << " C" << endl;
    cout << "Max Temperature: " << fToC(tail->nextTemp->wd->getTemp()) << " C" << endl << endl;
//    cout << "Top 1% of temperatures (>= " << determineTop(1) << "):" << endl; // index 1 corresponds to temperatures
//    printTop(determineTop(1));
//    cout << "Bottom 1% of temperatures (<= " << determineBottom(1) << "):" << endl;
//    printBottom(determineBottom(1));
    cout << "----------------------------------------------------------------------" << endl;
    cout << "WINDSPEED" << endl;
    cout << "Min windspeed: " << mphToMps(head->nextWind->wd->getWind()) << " m/s" <<endl;
    cout << "Max windspeed: " << mphToMps(tail->wd->getWind()) << "m/s" << endl;
//    cout << "Top 1% of windspeeds (>= " << determineTop(2) << "):" << endl;
//    printTop(determineTop(2));
//    cout << "Bottom 1% of windspeeds (<= " << determineBottom(2) << "):" << endl;
//    printBottom(determineBottom(2));
}

void LinkedList::printSmallReport() {
    int time_min = head->nextTime->wd->getTime(), time_max = tail->nextTime->wd->getTime();
    double temp_min = head->nextTemp->wd->getTemp(), temp_max = tail->nextTemp->wd->getTemp(),
    wind_min = head->nextWind->wd->getWind(), wind_max = tail->nextWind->wd->getWind();

    cout << endl << "\t\t\t\t\t\t\t------ Data Report ------\t\t\t\t\t\t\t" << endl;
    cout << "Total data points: " << size << endl << endl;
    cout << "Time Range:\t(" << head->nextTime->wd->getTime() << "-" << tail->nextTime->wd->getTime() << ")" << endl;
    cout << "Time Min: " << time_min << "\nTime Max: " << time_max << "\n1% of temps: " << ((time_max - time_min) * 0.01) << endl << endl;

    cout << "Temp Range:\t(" << head->nextTemp->wd->getTemp() << "-" << tail->nextTemp->wd->getTemp() << ")" << endl;
    cout << "Temp Min: " << temp_min << "\nTemp Max: " << temp_max << "\n1% of times: " << ((temp_max - temp_min) * 0.01) << endl << endl;

    cout << "Wind Range:\t(" << head->nextWind->wd->getWind() << "-" << tail->nextWind->wd->getWind() << ")" << endl;
    cout << "Wind Min: " << wind_min << "\nWind Max: " << wind_max << "\n1% of winds: " << ((wind_max - wind_min) * 0.01) << endl << endl;
}
void LinkedList::prettyPrint() const {
    Node *curr = head->nextTime;
    cout << "List size: " << size << endl;//"\n[";
    if (size > 10) {
        int i = 0;
        while (curr) {
            if ((i < 5 || (i > size - 6))) {
                cout << curr->wd->getTime() << ", " << curr->wd->getTemp() << ", " << curr->wd->getWind() << endl;
                if (curr->nextTime) {
                    cout << (i == 4 ? " ...\n" : "");
                }
            }
            curr = curr->nextTime;
            i++;
        }
    }
}

// Used for debugging
void LinkedList::uglyPrint(bool slightlyLessUgly) const {
    Node *curr = head->nextTime;
    cout << endl << "List size: " << size << (slightlyLessUgly ? "\n[" : "\n");
    while (curr) {
        cout << curr->wd->getTime() << ", " << curr->wd->getTemp() << ", " << curr->wd->getWind() << "\a";
        cout << (curr->nextTime ? ", " : "");
        curr = curr->nextTime;
    }
    cout << (slightlyLessUgly ? "]" : "");
}

bool LinkedList::addSorted(Node *&new_node) {
    if(linkSortedTime(head->nextTime, new_node)) {
        bool ret = linkSortedTemp(head->nextTemp, new_node);
        ret |= !linkSortedWind(head->nextWind, new_node);
        ++size;
        return ret;
    }
    return false;
}

bool LinkedList::linkSortedTime(Node *& curr, Node *& new_node) {
    if(!curr) {
        curr = new_node;
        tail->nextTime = new_node;
        return true;
    }
    if(curr->wd->getTime() == new_node->wd->getTime()) {
        return false;
    } else if(curr->nextTime && curr->nextTime->wd->getTime() > new_node->wd->getTime()) {
        new_node->nextTime = curr->nextTime;
        curr->nextTime = new_node;
        return true;
    }
    return linkSortedTime(curr->nextTime, new_node);
}

bool LinkedList::linkSortedTemp(Node *& curr, Node *& new_node) {
    if(!curr) {
        curr = new_node;
        tail->nextTemp = new_node;
        return true;
    }
    if(curr->nextTemp && curr->nextTemp->wd->getTemp() > new_node->wd->getTemp()) {
        new_node->nextTemp = curr->nextTemp;
        curr->nextTemp = new_node;
        return true;
    }
    return linkSortedTemp(curr->nextTemp, new_node);
}

bool LinkedList::linkSortedWind(Node *& curr, Node *& new_node) {
    if(!curr) {
        curr = new_node;
        tail->nextWind = new_node;
        return true;
    }
    if(curr->nextWind && curr->nextWind->wd->getWind() > new_node->wd->getWind()) {
        new_node->nextWind = curr->nextWind;
        curr->nextWind = new_node;
        return true;
    }
    return linkSortedWind(curr->nextWind, new_node);
}

// Deprecated AF
void LinkedList::removeDupes() {
    Node *curr = head->nextTime, *trailCurr = head, *temp = nullptr;
    if (curr->remove) {
        temp = curr;
        curr = curr->nextTime;
        delete temp;
        trailCurr->nextTime = curr;
        size--;
    }

    while (curr->nextTime) {
        if (curr->remove) {
            temp = curr;
            curr = curr->nextTime;
            delete temp;
            trailCurr->nextTime = curr;
            size--;
        } else {
            trailCurr = curr;
            curr = curr->nextTime;
        }
    }

    if (curr->remove) {
        delete curr;
        tail = trailCurr;
        size--;
    }
};

void LinkedList::destroy(Node *curr) {
    if(!curr) {
        return;
    }
    destroy(curr->nextTime);
    delete curr;
    curr = nullptr;
    return;
}

bool LinkedList::copy(Node *&result, Node *source) {
    if(!source) {
        result = nullptr;
        return true;
    }
    result = new Node(*source);
    copy(result->nextTime, source->nextTime);
    return true;
}


// ---------------- Helper Functions -----------------
double fToC(double f) {
    return (f - 32.0) * (5.0/9.0);
}

double mphToMps(double mph) {
    return mph * 0.44704;
}

double LinkedList::determineBottom(int top, int bottom, int size) const {
    // Pass for now.
}

double LinkedList::determineTop(int top, int bottom, int size) const {
    // Pass for now
}

void LinkedList::printTop(double top) const {
    Node *curr = head->nextTime;
    while (curr) {
        if (curr->wd->getTemp() >= top) {
            cout << "Timestamp: " << curr->wd->getTime() << "\nTemperature: " << fToC(curr->wd->getTemp())
                 << " C\nWindspeed: " << mphToMps(curr->wd->getWind()) << " m/s" << endl << endl;
        }
        curr = curr->nextTime;
    }
}

void LinkedList::printBottom(double bottom) const {
    Node *curr = head->nextTime;
    while (curr) {
        if (curr->wd->getTemp() <= bottom) {
            cout << "Timestamp: " << curr->wd->getTime() << "\nTemperature: " << fToC(curr->wd->getTemp())
                 << " C\nWindspeed: " << mphToMps(curr->wd->getWind()) << " m/s" << endl << endl;
        }
        curr = curr->nextTime;
    }
}


