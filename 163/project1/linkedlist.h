#ifndef LINKEDLIST_H
#define LINKEDLIST_H

#include <iostream>
#include "weatherdata.h"

class Node {
public:
    Node();
    Node(WeatherData *);
    ~Node();
    Node(const Node&);
    WeatherData *wd;
    Node *nextTime;
    Node *nextTemp;
    Node *nextWind;
    bool remove;
    void print();
};

class LinkedList {
public:
    LinkedList();
    ~LinkedList();
    LinkedList(const LinkedList &);

    double determineBottom(int, int, int) const;
    double determineTop(int, int, int) const;

    void generateReport() const;
    void printTop(double) const;
    void printBottom(double) const;
    void prettyPrint() const;
    void uglyPrint(bool) const;

    bool addSorted(Node *&);
    void removeDupes();
    void printSmallReport();

private:
    int size;
    Node *head, *tail;
    bool linkSortedTime(Node *&, Node *&);
    bool linkSortedTemp(Node *&, Node *&);
    bool linkSortedWind(Node *&, Node *&);

    void destroy(Node *);
    bool copy(Node *&, Node *);
};

#endif