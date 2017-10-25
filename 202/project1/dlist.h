/**
 * @file dlist.h
 * @brief Header file for class and method definitions of a doubly-linked list class.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef DLIST_H
#define DLIST_H

#include "dnode.h"

class DLL {
public:
    DLL();
    ~DLL();
    DLL(const DLL &);
    void display();
    bool add(FireSuppression *&);
    void destroy(D_Node *);
protected:
    D_Node *_head;
    int recursive_display(D_Node *);
    bool recursive_copy(D_Node *&, D_Node *);
};
#endif
