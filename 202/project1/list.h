/**
 * @file list.h
 * @brief Header file for class and method definitions of doubly-linked and linearly-linked list
 * classes DLL and LLL, respectively.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef LIST_H
#define LIST_H

#include "node.h"

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
