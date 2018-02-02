#include <iostream>

#include "node.h"

using namespace std;

Node::Node() {}

Node::Node(const family anItem) : item(anItem), next(nullptr) {}

Node::Node(const family& anItem, Node* nextNodePtr) : item(anItem), next(nextNodePtr) {}

void Node::setItem(const family& anItem) {
    item = anItem;
}

void Node::setNext(Node* nextNodePtr) {
    next = nextNodePtr;
}

family Node::getItem() const {
    return item;
}

Node* Node::getNext() const {
    if (next) {
        return next;
    } else {
        return nullptr;
    }
}


