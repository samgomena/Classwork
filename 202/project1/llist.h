/**
 * @file llist.h
 * @brief Header file for class and method definitions of a linearly-linked list.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef LLIST_H
#define LLIST_H

#include "lnode.h"

class LLL {
    public:
        LLL();
        ~LLL();
        LLL(const LLL &);
        void display();
        bool add(int);
        void build(int);
        void destroy(L_Node *);
    protected:
        L_Node *_head;
        int recursive_display(L_Node *);
        bool recursive_copy(L_Node *&, L_Node *);
};

#endif
