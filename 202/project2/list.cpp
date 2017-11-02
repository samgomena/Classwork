/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "list.h"

Node::Node(int study_type) : next(NULL), study_type(study_type), materials(NULL) {
    switch(study_type) {
        case 0:
            materials = new Textbook();
            break;
        case 1:
            materials = new Programming();
            break;
        case 2:
            materials = new Slides();
            break;
        default:
            // Materials could be left NULL. Make sure to check.
            break;
    }
}

Node::~Node() {
    if(materials) {
        delete materials;
    }
}

Node::Node(const Node &copy_me) : next(NULL), study_type(copy_me.study_type), materials(copy_me.materials) {}

 StudyMaterials& Node::data() {
     if(materials) {
         return *materials;
     }
 }

void Node::print() const {
    if(materials) {
        materials->display();
    }
}

const int Node::studies() const {
    return study_type;
}

// -----------------------------------------------------
// List definition
// -----------------------------------------------------

CLL::CLL() : rear(NULL), review_questions(NULL), size(3) {
    // [0] = number of textbooks, [1] = number of practice questions, [2] = number of slides.
    review_questions = prompt_user(size);
    for(int i = 0; i < size; i++) {
        for(int j = 0; j < review_questions[i]; j++) {
            Node *new_node = new Node(i);
            add(new_node);
        }
    }
}

CLL::~CLL() {
    delete review_questions;
    review_questions = NULL;

    if(rear) {
        destroy(rear->next);
    }
    delete rear;
    rear =  NULL;
}

CLL::CLL(const CLL& copy_me) : rear(NULL), review_questions(copy_me.review_questions), size(copy_me.size) {
    // Recursive copy
    copy(rear, copy_me.rear);
}

bool CLL::add(Node *& new_node) {
    if(!rear) {
        rear = new_node;
        new_node->next = rear;
        return true;
    }
    Node *temp = rear->next;
    rear->next = new_node;
    new_node->next = temp;
    rear = new_node;
    return true;
}

Node & CLL::retrieve_first(const char * retrieve_first) {
    int index = find_index(rear->next, retrieve_first);
    if(index > 0) {
        return find_node(rear->next, index, 0);
    }
}

bool CLL::remove_first(const char * remove_first) {
    int index = find_index(rear->next, remove_first);
    if(index > 0) {
        return remove_node(rear->next, index, 0);
    } else {
        return false;
    }
}

void CLL::display() {
    if(!rear) {
        return;
    }
    r_display(rear->next);
    rear->print();
}

void CLL::r_display(Node *curr) {
    if(curr == rear) {
        return;
    }
    curr->print();
    r_display(curr->next);
    return;
}

void CLL::remove_all() {
    if(rear) {
        destroy(rear->next);
    }
    delete rear;
    rear = NULL;
}

int CLL::find_index(Node * curr, const char * find_me) {
    if(curr == rear) {
        return 0;
    }
    if(strcmp(curr->data().data(), find_me) == 0) {
        return 0;
    }
    return 1 + find_index(curr->next, find_me);
}

void CLL::destroy(Node *& rear) {
    if(rear == this->rear) {
        return;
    }
    destroy(rear->next);
    delete rear;
    rear = NULL;
    return;
}

void CLL::copy(Node *& dest, Node * src) {
    if(!src) {
        dest = NULL;
        return;
    }
    dest = new Node(*src);
    copy(dest->next, src->next);
    return;
}

Node & CLL::find_node(Node *& curr, int find_index, int curr_index) {
    if(find_index == curr_index) {
        return *curr;
    }
    find_node(curr->next, find_index, curr_index + 1);
}

bool CLL::remove_node(Node *& curr, int remove_index, int curr_index) {
    if(curr_index == remove_index) {
        Node * temp = curr;
        curr = curr->next;
        delete temp;
        temp = NULL;
        return true;
    }
    return remove_node(curr->next, remove_index, curr_index + 1);
}


