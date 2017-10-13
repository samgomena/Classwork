/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "node.h"

// ----------------------------------------------------------
// D_NODE
// ----------------------------------------------------------
int D_Node::uuid = 0;
D_Node::D_Node() : _next(NULL), _prev(NULL), data(0), uid(uuid++) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " constructor called!" << endl;    
    }
}
D_Node::D_Node(int init_data) : _next(NULL), _prev(NULL), uid(uuid++), data(init_data){
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " constructor called with " << init_data << endl;
    }
}
D_Node::~D_Node() {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node" << uid << " destructor called" << endl;  
    }
    
}
D_Node::D_Node(const D_Node &to_copy) : _next(to_copy._next), _prev(to_copy._prev), data(to_copy.data), uid(uuid) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " copy constructor called." << endl;
    }
    
}
int D_Node::get_data() {
    return data;
}
D_Node *& D_Node::next() {
    return _next;
}
void D_Node::next(D_Node * new_next) {
    _next = new_next;
}
D_Node * D_Node::prev() {
    return _prev;
}
void D_Node::prev(D_Node * new_prev) {
    _prev = new_prev;
}

// ----------------------------------------------------------
// L_NODE
// ----------------------------------------------------------
int L_Node::uuid = 0;
L_Node::L_Node() : _next(NULL), data(0), uid(uuid++) {
    if(DEBUG) {
        NOTICE();
        cout << "L_Node " << uid << " constructor called!" << endl;    
    }
}
L_Node::L_Node(int init_data) : _next(NULL), uid(uuid++), data(init_data){
    if(DEBUG) {
        NOTICE();
        cout << "L_Node " << uid << " constructor called with " << init_data << endl;
    }
}
L_Node::~L_Node() {
    if(DEBUG) {
        NOTICE();
        cout << "L_Node" << uid << " destructor called" << endl;  
    }
    
}
L_Node::L_Node(const L_Node &to_copy) : _next(to_copy._next), data(to_copy.data), uid(uuid) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " copy constructor called." << endl;
    }
    
}
int L_Node::get_data() {
    return data;
}
L_Node *& L_Node::next() {
    return _next;
}
void L_Node::next(L_Node * new_next) {
    _next = new_next;
}
