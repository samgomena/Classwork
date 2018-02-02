#ifndef LINKEDLIST
#define LINKEDLIST

#include "family.h"

class Node {
private :
    family item;
    Node* next;
public :
    Node();
    Node(const family anItem);
    Node(const family& anItem, Node* nextNodePtr);
    void setItem(const family& anItem);
    void setNext(Node* nextNodePtr);
    family getItem() const;
    Node* getNext() const;
};
#endif