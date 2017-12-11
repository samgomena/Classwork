#include "queue.h"

Queue::Queue() : front(nullptr), back(nullptr) {}

Queue::~Queue() {
    while(!isEmpty()) {
        dequeue();
    }
}

bool Queue::isEmpty() const {
    return front == nullptr;
}

bool Queue::enqueue(const int newData) {
    Node* newNode = new Node(newData);
// Insert the new node
    if (isEmpty()) {
        front = newNode; // The queue was empty
    } else {
        back->next = newNode; // The queue was not empty
    }
    back = newNode; // New node is at back
    return true ;
}

bool Queue::dequeue() {
    bool result = false ;
    if (!isEmpty()) {
        // Queue is not empty; remove front
        Node* nodeToDelete = front;
        if (front == back) {
            // Special case: one node in queue
            front = nullptr ;
            back = nullptr ;
        } else {
            front = front->next;
        }
        // Return deleted node to system
        nodeToDelete->next = nullptr;
        delete nodeToDelete;
        nodeToDelete = nullptr ;
        result = true ;
    }
    return result;
}

int Queue::peek() const {
    if(!isEmpty()) {
        return front->getData();
    }
}
