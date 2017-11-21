/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "list.h"

ListNode::ListNode() : next(NULL), data_point(0.0) {}
ListNode::ListNode(float data_point) : next(NULL), data_point(data_point) {}

/**
 * @brief Prints the node's data to the console
 * @param out The console output object
 * @param node The node in question to print
 * @return ostream& The console output object
 */
ostream& operator << (ostream& out, const ListNode& node) {
    out << node.data_point;
    return out;
}
/**
 * @brief Get a nodes data_point value
 * @return float The data_point value
 */
const float ListNode::data() const {
    return data_point;
}
/**
 * @brief Set a nodes data_point value
 *
 * This function is only used for testing purposes.
 *
 * @param new_data The new data_point value
 */
void ListNode::data(float new_data) {
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

/**
 * @brief Creates a new node and adds it to the list in sorted order.
 *
 * Public wrapper that, after creating a new node, calls the protected add function.
 *
 * @param data_point The data to put in the new node
 * @return bool Whether we were able to add the node, this will always be true.
 */
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
    tail = NULL;
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

/**
 * @brief Adds a new node to the list.
 *
 * This function will determine if traversal is required to insert the node, if it's not, it will handle the
 * nodes insertion and return. Otherwise, it will call another protected add function to perform the traversal
 * and insertion.
 *
 * @param new_node The node to add to the list.
 * @return bool Whether we were able to add the node, this will always be true.
 */
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

/**
 * @brief Uses a recursive traversal to determine a nodes placement and then inserts.
 * @param curr The current node in the list. Used exclusively for traversal.
 * @param new_node The node to add to the list.
 * @return bool Whether we were able to add the node, this will always be true.
 */
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

/**
 * @brief Recursively display the data in a nice way.
 * @param curr The current node in the list. Used exclusively for traversal.
 * @return int The number of nodes in the list.
 */
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

/**
 * @brief Array access operator, overloaded for use with linked list.
 *
 * This is the version of the overloaded array access operator, it is used for setting data in the list.
 * I.e. list[0] = data;
 *
 * @param index The index at we want to get a node.
 * @return ListNode& The node at `index`.
 */
ListNode& Sol::operator[](int index) { // TODO: throw an error instead of return null
    return get_element_at(this->head, 0, index);
}

/**
 * @brief Array access operator, overloaded for use with linked list.
 *
 * This is the constant version of the overloaded array access operator, that is, it is used for getting data from the list.
 * I.e. data = list[0];
 *
 * @param index The index at we want to get a node.
 * @return ListNode& The node at `index`.
 */
const ListNode& Sol::operator[](int index) const { // TODO: throw an error instead of return null
    return get_element_at(this->head, 0, index);
}


Sol Sol::operator+(const Sol *& addend) {
    Sol *new_list = new Sol(*this);
    this->merge_list(new_list, addend->head);
    return *new_list;
}

// Private functions
/**
 * @brief Helper function used by the overloaded array-access operator.
 * @param curr The current node in the list. Used exclusively for traversal.
 * @param curr_index The current index in the list.
 * @param index The index at which we want to get a node.
 * @return ListNode& The node at `index`.
 */
ListNode& Sol::get_element_at(ListNode* curr, int curr_index, int index) const {
    if(!curr) {
        ListNode *temp = NULL;
        return *temp;
    }
    if(curr_index == index) {
        return *curr;
    }
    return get_element_at(curr->next, curr_index + 1, index);
}

void Sol::merge_list(Sol *& list1, ListNode *curr) {
    if(!curr) {
        return;
    }
    list1->add(curr);
    merge_list(list1, curr->next);
}



