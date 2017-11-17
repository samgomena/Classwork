#include "list.h"

ListNode::ListNode() : next(NULL) {}
ListNode::ListNode(float data_point) : next(NULL), data_point(data_point) {}
ostream& operator << (ostream& out, const ListNode& node) {
    out << node.data_point;
}

Sol::Sol() : head(NULL), tail(NULL) {}
Sol::~Sol() {
    if(head) {
        destroy(head);
    }
}
Sol::Sol(const Sol& copy) : head(NULL), tail(NULL) {
    if(&copy) {
        Sol::copy(head, copy.head);
    }
}

Sol& Sol::operator=(const Sol& equal) {
    if(this == &equal) {
        return *this;
    }
    if(head) {
        destroy(head);
    }
    copy(head, equal.head);
    return *this;
}

bool Sol::add(float data_point) {
    ListNode *node = new ListNode(data_point);
    add(node);
}

int Sol::display() {
    if(!head) {
        return 0;
    }
    return display(head);
}

// Protected
void Sol::destroy(ListNode *& head) {
    if(!head) {
        return;
    }
    destroy(head->next);
    delete head;
    head = NULL;
}

bool Sol::copy(ListNode *&result, ListNode *source) {
    if(!source) {
        result = NULL;
        return true;
    }
    result = new ListNode(*source);
    copy(result->next, source->next);
    return true;
}

bool Sol::add(ListNode *& new_node) {
    if(!head) {
        head = new_node;
        tail = new_node;
        return true;
    }
    new_node->next = head;
    head = new_node;
    return true;
}

int Sol::display(ListNode *curr) {
    if(!curr) {
        return 0;
    }
    cout << curr;
    return display(curr->next) + 1;
}
