#ifndef LINKEDLIST
#define LINKEDLIST

#include "family.h"

class Node {
private :
    family fam;
    Node* next;
public :
    Node();
    Node(const family anItem);
    Node(const family& anItem, Node* nextNodePtr);
    Node(const Node&);
    Node& operator=(const Node&);
    void setItem(const family& anItem);
    void setNext(Node* nextNodePtr);
    family getItem() const;
    Node* getNext() const;
};
#endif