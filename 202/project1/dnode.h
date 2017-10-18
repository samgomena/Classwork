/**
 * @file node.h
 * @brief Header file for class and method definitions of doubly linked list class.
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef DNODE_H
#define DNODE_H

#include "fire_suppression.h"
#include "utils.h"

class D_Node {
public:
    D_Node();
    ~D_Node();
    D_Node(const D_Node &);
    D_Node(FireSuppression *&);
    FireSuppression *& get_data();
    D_Node *& next();
    void next(D_Node *);
    D_Node * prev();
    void prev(D_Node *);
protected:
    D_Node *_next;
    D_Node *_prev;
    FireSuppression *fs;
private:
    // Unique id generator
    static int uuid;
    // constant id variable
    const int uid;
};

#endif
