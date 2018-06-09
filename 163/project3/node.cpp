#include <iostream>

#include "node.h"

using namespace std;

Node::Node() : next(nullptr) {}

Node::Node(const family anItem) : fam(anItem), next(nullptr) {}

Node::Node(const family& anItem, Node* nextNodePtr) : fam(anItem), next(nextNodePtr) {}

Node::Node(const Node& copy_node) : fam(copy_node.fam), next(copy_node.next) {}

Node& Node::operator=(const Node& equal_node) {
    if(this == &equal_node) {
        return *this;
    }
    this->fam = equal_node.fam;
}
void Node::setItem(const family& anItem) {
    fam = anItem;
}

void Node::setNext(Node* nextNodePtr) {
    next = nextNodePtr;
}

family Node::getItem() const {
    return fam;
}

Node* Node::getNext() const {
//    if (next) {
//        return next;
//    } else {
//        return nullptr;
//    }
    return next;// ? next : nullptr;
}


