#include "list.h"

ListNode::ListNode() : next(NULL) {}
ListNode::ListNode(float data_point) : next(NULL), data_point(data_point) {}
//ListNode::ListNode(const ListNode& copy) : next(NULL) {}
//ListNode& ListNode::operator=(const ListNode& equal) {
//    // TODO
//    if(this == &equal) {
//        return *this;
//    }
//    return *this;
//}

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
    tail->next = new_node;
    tail = new_node;
    return true;
}
