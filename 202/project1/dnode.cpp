/**
 * @file dnode.cpp
 * @brief Implementation of doubly linked list node class.
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "dnode.h"

int D_Node::uuid = 0;
D_Node::D_Node() : _next(NULL), _prev(NULL), fs(NULL), uid(uuid++) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " constructor called!" << endl;
    }
}
D_Node::D_Node(FireSuppression *& fs_unit) : _next(NULL), _prev(NULL), fs(fs_unit), uid(uuid++) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " constructor called." << endl;
    }
}
D_Node::~D_Node() {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node" << uid << " destructor called" << endl;
    }
}
D_Node::D_Node(const D_Node &to_copy) : _next(to_copy._next), _prev(to_copy._prev), fs(to_copy.fs), uid(uuid) {
    if(DEBUG) {
        NOTICE();
        cout << "D_Node " << uid << " copy constructor called." << endl;
    }
}
/**
 * @brief Get the nodes data.
 *
 * @return FireSuppression The nodes fire suppression object.
 */
FireSuppression *& D_Node::get_data() {
    return fs;
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