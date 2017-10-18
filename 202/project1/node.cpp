/**
 * @file node.cpp
 * @brief Implementation of doubly linked list node and linearly linked list node classes, respectively.
 *
 * @author Sam Gomena
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
/**
 * @brief Get the nodes data member.
 *
 * @return int The nodes data member
 */
int D_Node::get_data() {
    return data;
}
/**
 * @breif Get the next node (by reference).
 *
 * @return D_Node *& The address of the next node.
 */
D_Node *& D_Node::next() {
    return _next;
}
/**
 * @brief Set the value of the current nodes next pointer.
 *
 * @param new_next The pointer to set the current nodes next pointer to.
 * @return void
 */
void D_Node::next(D_Node * new_next) {
    _next = new_next;
}
/**
 * @breif Get the previous node (by reference).
 *
 * @return D_Node *& The address of the previous node.
 */
D_Node * D_Node::prev() {
    return _prev;
}
/**
 * @brief Set the value of the current nodes previous pointer.
 *
 * @param new_prev The pointer to set the current nodes previous pointer to.
 * @return void
 */
void D_Node::prev(D_Node * new_prev) {
    _prev = new_prev;
}

// ----------------------------------------------------------
// L_NODE
// ----------------------------------------------------------
// Initialize unique id generator. This is used for testing.
int L_Node::uuid = 0;
L_Node::L_Node() : _next(NULL), uid(uuid++), city(uid) {
    if(DEBUG) {
        NOTICE();
        cout << "L_Node " << uid << " constructor called!" << endl;    
    }
}
L_Node::L_Node(int city_id) : _next(NULL), uid(uuid++), city(city_id){
    if(DEBUG) {
        NOTICE();
        cout << "L_Node " << uid << " constructor called with " << endl;
    }
}
L_Node::~L_Node() {
    if(DEBUG) {
        NOTICE();
        cout << "L_Node" << uid << " destructor called" << endl;  
    }
    
}
L_Node::L_Node(const L_Node &to_copy) : _next(to_copy._next), city(to_copy.city), uid(uuid) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " copy constructor called." << endl;
    }
    
}
/**
 * @brief Display's the contents of the city data member.
 *
 * This function is a wrapper to display the city class. As an aside, I realize there are *much* better ways to
 * perform this (like a display function in the city class)! However, none of those weren't implemented
 * when this was due.
 *
 * @return void
 */
void L_Node::display_city() {
    cout << city;
}
/**
 * @breif Get the next node (by reference).
 *
 * @return L_Node *& The address of the next node.
 */
L_Node *& L_Node::next() {
    return _next;
}
/**
 * @brief Set the value of the current nodes next pointer.
 *
 * @param new_next The pointer to set the current nodes next pointer to.
 * @return void
 */
void L_Node::next(L_Node * new_next) {
    _next = new_next;
}
