/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "list.h"

ListNode::ListNode() : next(NULL), data_point(0.0) {}
ListNode::ListNode(float data_point) : next(NULL), data_point(data_point) {}
ostream& operator << (ostream& out, const ListNode& node) {
    out << node.data_point;
    return out;
}

const float ListNode::data() const {
    return data_point;
}

float ListNode::data(float new_data) {
    data_point = new_data;
}

bool ListNode::operator==(const ListNode* cmp) const {
    return this->data() == cmp->data();
}

bool ListNode::operator!=(const ListNode* cmp) const {
    return this->data() != cmp->data();
}

bool ListNode::operator>(const ListNode* cmp) const {
    return this->data() > cmp->data();
}

bool ListNode::operator>=(const ListNode* cmp) const {
    return this->data() >= cmp->data();
}

bool ListNode::operator<=(const ListNode *cmp) const {
    return this->data() <= cmp->data();
}

bool ListNode::operator<(const ListNode *cmp) const {
    return this->data() < cmp->data();
}

// List Iplementation
Sol::Sol() : head(NULL), tail(NULL) {}
Sol::~Sol() {
    if(head) {
        destroy(head);
    }
}
Sol::Sol(const Sol& copy) : head(NULL), tail(NULL) {
    Sol::copy(head, copy.head);
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
    return add(node);
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

//bool Sol::add(ListNode *& new_node) {
//    if(!head) {
//        head = new_node;
//        tail = new_node;
//        return true;
//    }
//    new_node->next = head;
//    head = new_node;
//    return true;
//}

bool Sol::add(ListNode *& new_node) {
    if(!head) {
        head = new_node;
        tail = new_node;
        return true;
    } else if(*new_node < head) {
        new_node->next = head;
        head = new_node;
        return true;
    }
    return add(head, new_node);
}

bool Sol::add(ListNode *&curr, ListNode *& new_node) {
    if(!curr) {
        curr = new_node;
        tail = new_node;
        return true;
    }  else if(curr->next && *curr->next > new_node) {
//        ListNode *temp = head->next;
        new_node->next = curr->next;
        curr->next = new_node;
        return true;
    }
    return add(curr->next, new_node);
}

int Sol::display(ListNode *curr) {
    if(!curr) {
        return 0;
    }
    if(!curr->next) {
        cout << *curr << endl;
    } else {
        cout << *curr << ", ";
    }
    return display(curr->next) + 1;
}

ListNode& Sol::operator[](int index) {
    return get_element_at(this->head, 0, index);
}

const ListNode& Sol::operator[](int index) const {
    return get_element_at(this->head, 0, index);
}

ListNode& Sol::get_element_at(ListNode* curr, int curr_index, int index) const {
    if(!curr) {
        ListNode *temp;
        return *temp;
    }
    if(curr_index == index) {
        return *curr;
    }
    return get_element_at(curr->next, curr_index + 1, index);
}
