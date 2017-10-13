/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

// #include <iostream>
#include <stdlib.h>     /* srand, rand */
#include <time.h>

#include "node.h"
#include "fire_suppression.h"

using namespace std;

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

// ----------------------------------------------------------
// DLL
// ----------------------------------------------------------
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

void DLL::display() {
    if(DEBUG) {
        NOTICE();
        cout << "DLL display called." << endl;    
    }
    if(_head) {
        cout << "DLL: ";
        int length = recursive_display(_head);
        cout << endl << "DLL is " << length << " items long." << endl;
    } else {
        cout << "DLL is empty!" << endl;
    }
    
}
void DLL::destroy(D_Node *head) {
    if(!head) {
        return;
    }
    destroy(head->next());
    delete head;
    head = NULL;
}
int DLL::recursive_display(D_Node * _head) {
    if(!_head) {
        return 0;
    }
    // Pretty print for the ladies
    if(!_head->next() && DEBUG) {
        cout << _head->prev() << " <--|("<<_head<<") " << _head->get_data() << "|--> " << _head->next();        
    } else if(!_head->next()) {
        cout << _head->get_data();
    } else if(DEBUG) {
        cout << _head->prev() << " <--|("<<_head<<") " << _head->get_data() << "|--> " << _head->next() << " ... ";
    } else {
        cout << _head->get_data() << " -> ";    
    }
    return 1 + recursive_display(_head->next());
}

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

void DLL::build(int length) {
    int i = 1;
    if(length == 1) {
        i = 0;
    } 
    for(; i <= length; i++) {
        int rand = generate_random(1, 100);
        add(rand);
    }
}

bool DLL::add(int data) {
    if(DEBUG) {
        NOTICE();
        cout << "DLL add called." << endl;    
    }
    D_Node *new_node = new D_Node(data);
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

// ----------------------------------------------------------
// LLL
// ----------------------------------------------------------
LLL::LLL() : _head(NULL) {
    if(DEBUG) {
        NOTICE();
        cout << "LLL constructor called." << endl;
    }
}
LLL::~LLL() {
    if(DEBUG) {
        NOTICE();
        cout << "LLL destructor called." << endl;
    }
    destroy(_head);
}
LLL::LLL(const LLL &to_copy) {
    if(DEBUG) {
        NOTICE();
        cout << "LLL copy constructor called." << endl;    
    }
    recursive_copy(_head, to_copy._head);
}

bool LLL::recursive_copy(L_Node *&result, L_Node *source) {
    if(!source) {
        result = NULL;
        return true;
    }
    result = new L_Node(*source);
    recursive_copy(result->next(), source->next());
    return true;    
}

void LLL::display() {
    if(DEBUG) {
        NOTICE();
        cout << "LLL display called." << endl;    
    }
    if(_head) {
        cout << "LLL: ";
        int length = recursive_display(_head);
        cout << endl << "LLL is " << length << " items long." << endl;
    } else {
        cout << "LLL is empty!" << endl;
    }
    
}

void LLL::destroy(L_Node *head) {
    if(!head) {
        return;
    }
    destroy(head->next());
    delete head;
    head = NULL;
}

int LLL::recursive_display(L_Node * _head) {
    if(!_head) {
        return 0;
    }
    // Pretty print 
    if(!_head->next()) {
        cout << _head->get_data();
    } else {
        cout << _head->get_data() << " -> ";    
    }
    return 1 + recursive_display(_head->next());
}

void LLL::build(int length) {
    int i = 1;
    if(length == 1) {
        i = 0;
    } 
    for(; i <= length; i++) {
        int rand = generate_random(1, 100);
        add(rand);
    }
}

bool LLL::add(int data) {
    if(DEBUG) {
        NOTICE();
        cout << "LLL add called." << endl;    
    }
    L_Node *new_node = new L_Node(data);
    L_Node *curr = _head;
    if(!_head) {
        _head = new_node;
        return true;
    }
    while(curr->next()) {
        curr = curr->next();
    }
    curr->next(new_node);
    return true;
}

int main() {
    srand(time(NULL));
    int dlist_length = 5;//generate_random(1, 20);
    int llist_length = generate_random(1, 20);
    cout << "List length: " << dlist_length << endl;

    // Test copy ctor    
    // D_Node *node = new D_Node(69);
    // D_Node *other_node = new D_Node(*node);
    // cout << "Node: " << node->get_data() << " " << node->prev() << "\nOther node: " << other_node->get_data() << " " << other_node->prev() << endl;
    
    DLL *list = new DLL();
    list->build(dlist_length);
    list->display();
    cout << endl << endl;
    // delete list;
    // list = NULL;
    
    // Test copy ctor
    DLL *nu_list = new DLL(*list);
    list->add(69);
    list->display();
    cout << endl << endl;
    nu_list->display();
    delete nu_list;
    nu_list = NULL;
    
    cout << endl << endl << "List length: " << llist_length << endl;
    LLL *l_list = new LLL();
    l_list->build(llist_length);
    l_list->display();
    delete l_list;
    list = NULL;
    
    // Test copy ctor
    // LLL *nu_list = new LLL(*l_list);
    // nu_list->display();
    // delete nu_list;
    // nu_list = NULL;
    
    // int i;
    // LLL **arr;
    // arr = new LLL*[10];
    // for(i = 0; i < 10; ++i) {
    //     arr[i] = new LLL();
    //     arr[i]->build(generate_random(1, 10));
    // }
    
    // for(i = 0; i < 10; ++i) {
    //     cout << "LLL " << i << ": ";
    //     arr[i]->display();
    //     delete arr[i];
    //     arr[i] = NULL;
    // }
    // delete arr;
    // arr = NULL;

    FireSuppression *areal = new Areal();
    FireSuppression *structural = new Structural();
    FireSuppression *wild = new Wild();
    
    areal->report();
    structural->report();
    wild->report();
    
    delete areal;
    delete structural;
    delete wild;
}





















