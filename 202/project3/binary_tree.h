#ifndef BINARY_TREE_H
#define BINARY_TREE_H

#include <fstream>
#include <cstring>
#include <cstdlib>
#include "list.h"

class Node {
public:
    Node();
    Node(int, int, float, int);
    ~Node();
    Node(const Node&);
    Node& operator=(const Node&);
    void add(float); // Wrapper for list add
    Node *left;
    Node *right;
    int days_to_race;
    static int uuid;
protected:
    int train_type;
    float workouts_per_week;
    int race_type;
    const int uid;

protected:
    Sol *race_history;
};

class BST {
public:
    BST();
    ~BST();
    BST(const BST&);
    BST& operator=(const BST&);
    bool add(int, int, float, int, char *); // Wrapper for protected add

protected:
    Node * root;
    void destroy(Node *&);
    void copy(Node *&, Node *);
    bool add(Node *, Node *&, int);
private:
    const char * DATA_FILE = "data.txt";
    void import();
};

#endif
