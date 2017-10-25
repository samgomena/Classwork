/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef LIST_H
#define LIST_H

#include <iostream>
#include "study_materials.h"

using namespace std;

class Node {
    public:
        Node(int);
        ~Node();
        Node(const Node &);
        StudyMaterials& data();
        void print() const;
        // Node *& next();
        // void next(Node *);
        Node *next;
    protected:
        StudyMaterials *materials;
        
};

class CLL {
public:
    CLL();
    ~CLL();
    CLL(const CLL&);
    bool add(Node *&);
    void retrieve();
    void remove();
    void remove_all();
    void display();
    // Recursive display
    void r_display(Node *);
protected:
    Node *rear;
    int *review_questions;
};

#endif