/**
 * @file dlist.cpp
 * @brief Implementation of doubly linked list class.
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "dlist.h"

DLL::DLL() : _head(NULL) {
    if(DEBUG) {
        NOTICE();
        cout << "DLL constructor called." << endl;
    }
}
DLL::~DLL() {
    if(DEBUG) {
        NOTICE();
        cout << "DLL destructor called." << endl;
    }
    destroy(_head);
}
DLL::DLL(const DLL &to_copy) : _head(NULL) {
    if(DEBUG) {
        NOTICE();
        cout << "DLL copy constructor called." << endl;
    }
    bool copy_result = recursive_copy(_head, to_copy._head);
    if(copy_result && DEBUG) {
        NOTICE();
        cout << "DLL copy was successful." << endl;
    } else if(!copy_result && DEBUG) {
        WARN();
        cout << "DLL copy was unnsuccessful.";
    }
}
/**
 * @brief Displays the contents of the list.
 *
 * A wrapper function used to expose the underlying recursive display implementation.
 *
 * @return void
 */
void DLL::display() {
    if(DEBUG) {
        NOTICE();
        cout << "DLL display called." << endl;
    }
    if(_head) {
        int length = recursive_display(_head);
        cout << "There are " << length << " fire suppression units available." << endl;
    } else {
        cout << "There are no fire suppression units available." << endl;
    }

}
/**
 * @brief Destroys the list (i.e. frees all nodes memory)
 *
 * This function is used by the destructor to recursively delete every node after the node
 *  passed in. Because the constructor is the only consumer to this function, the parameter
 *  is named head as it will always pass this->head in.
 *
 * @param head The node to begin recursive deletion on. Typically, this node is the head of the list.
 * @return void
 */
void DLL::destroy(D_Node *head) {
    if(!head) {
        return;
    }
    destroy(head->next());
    delete head;
    head = NULL;
}
/**
 * @brief Recursively displays the contents of the list.
 *
 * Function that is consumed by the public `display()` function. This was done to preserve
 * `_head`'s protected status.
 *
 * @param _head The node to begin recursively displaying on. Typically, we want to display
 * the whole list, hence its name.
 *
 * @return int The number of nodes in the list.
 */
int DLL::recursive_display(D_Node * _head) {
    if(!_head) {
        return 0;
    }

    cout << _head->get_data() << " -> ";
    return 1 + recursive_display(_head->next());
}
/**
 * @brief Recursively copy `source` into `dest`.
 *
 * Function that is consumed by the DLL classes copy constructor for ease of recursive
 * implementation.
 *
 * @param dest The destination list that will be produced during copy.
 * @param source The list we are copying into `dest`.
 *
 * @return bool The status of our copying operation.
 */
bool DLL::recursive_copy(D_Node *& dest, D_Node *source) {
    bool result = true;
    if(!source) {
        dest = NULL;
        return result;
    }
    dest = new D_Node(source->get_data());

    result = recursive_copy(dest->next(), source->next());
    if(dest->next()) {
        dest->next()->prev(dest);
    }
    return result;
}
/**
 * @brief Adds `data` to the end of the list.
 *
 * General add function for the list. Analagous to `push_back(...)` with vectors.
 *
 * @param data The data to add to the list
 *
 * @return bool The result of adding `data` to the list.
 */
bool DLL::add(FireSuppression *& fs_data) {
    if(DEBUG) {
        NOTICE();
        cout << "DLL add called." << endl;
    }
    D_Node *new_node = new D_Node(fs_data);
    D_Node *curr = _head;
    if(!_head) {
        _head = new_node;
        new_node->prev(_head);
        return true;
    }
    while(curr->next()) {
        curr = curr->next();
    }
    curr->next(new_node);
    new_node->prev(curr);
    return true;
}


