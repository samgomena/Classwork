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

class D_Node {
    public:
        D_Node();
        ~D_Node();
        D_Node(const D_Node &);
        D_Node(int data);
        int get_data();
        D_Node *& next();
        void next(D_Node *);
        D_Node * prev();
        void prev(D_Node *);
    protected:
        int data;
        D_Node * _next;
        D_Node *_prev;
    private:
        // Unique id generator
        static int uuid;
        // constant id variable
        const int uid;
};

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
