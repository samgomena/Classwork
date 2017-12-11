#include "stack.h"
#include <cassert>

Stack::Stack() : top(nullptr) {}

Stack::~Stack() {
    while(!isEmpty()) {
        pop();
    }
}

bool Stack::isEmpty() const {
    return top == nullptr;
}

bool Stack::push(int x, int y) {
    Node *newNode = new Node(x, y);
    newNode->next = top;
    top = newNode;
    return true;
}

bool Stack::pop() {
    bool result = false;
    if (!isEmpty()) {
        Node* nodeToDelete = top;
        top = top->next;
//        nodeToDelete->next = nullptr;
        delete nodeToDelete;
        nodeToDelete = nullptr;
        result = true;
    }
    return result;
}

int Stack::peekX() const {
    assert(!isEmpty());
    return top->getX();
}

int Stack::peekY() const {
    assert(!isEmpty());
    return top->getY();
}
