/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 *
 * Purpose:
 *      A header file to store node classes for doubly linked lists and linear linked lists. Hence their names `DLL` and `LLL`, respectively.

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
        bool add(int);
        void build(int);
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
