/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef LIST_H
#define LIST_H

#include "study_materials.h"

using namespace std;

class Node {
public:
    Node(int);
    ~Node();
    Node(const Node &);
    StudyMaterials& data();
    void print() const;
    const int studies() const;
    // Node *& next();
    // void next(Node *);
    Node *next;
protected:
    const int study_type;
    StudyMaterials *materials;
};

class CLL {
public:
    CLL();
    ~CLL();
    CLL(const CLL &);
    bool add(Node *&);
    Node &retrieve_first(const char *);
    bool remove_first(const char *);
    void remove_all();
    void display();
    void r_display(Node *); // Recursive display
protected:
    Node *rear;
    int *review_questions;

    int find_index(Node *, const char *);
    void destroy(Node *&);
    void copy(Node *&, Node *);
private:
    const int size;  // Number of questions to answer
    Node & find_node(Node *&, int, int);
    bool remove_node(Node *&, int, int);
};

#endif