/**
 * @file node.h
 * @brief Header file for class and method definitions of doubly linked list and linearly linked list classes, respectively.
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef NODE_H
#define NODE_H

#include <iostream>
#include "location.h"

using namespace std;

class L_Node {
    public:
        L_Node();
        ~L_Node();
        L_Node(const L_Node &);
        L_Node(int);
        void display_city();
        L_Node *& next();
        void next(L_Node *);
    private:
        static int uuid;
    protected:
        L_Node *_next;
        const int uid;
        City city;
};

#endif
