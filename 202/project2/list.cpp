/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "list.h"

Node::Node(int study_type) : materials(NULL), next(NULL) {
    switch(study_type) {
        case 0:
            materials = new Textbook();
            break;
        case 1:
            materials = new Programming();
            break;
        case 2:
            materials = new Slides();
    }
}

Node::~Node() {
    delete materials;
}

Node::Node(const Node &copy_me) : materials(copy_me.materials), next(NULL) {}

// StudyMaterials& Node::Data() {
//     return materials;
// }

void Node::print() const {
    materials->display();
} 

// -----------------------------------------------------
// List definition
// -----------------------------------------------------

CLL::CLL() : rear(NULL), review_questions(NULL) {
    // [0] = number of textbooks, [1] = number of slides, [2] = number of practice questions.
    int *review_questions = prompt_user();
    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < review_questions[i]; j++) {
            // cout << review_questions[i] << endl;
            Node *new_node = new Node(i);
            add(new_node);
        }
    }
    delete []review_questions;
}

CLL::~CLL() {
    delete review_questions;
    review_questions = NULL;
}

CLL::CLL(const CLL& copy_me) : rear(NULL), review_questions(copy_me.review_questions) {
    // Recursive copy
}


bool CLL::add(Node *& new_node) {
    if(!rear) {
        rear = new_node;
        new_node->next = rear;
        return true;
    }
    Node *temp = rear;
    rear->next = new_node;
    new_node->next = temp;
    rear = new_node;
    return true;
}

void CLL::display() {
    cout << "CLL DISPLAY\nRear: " << rear << endl;
    if(!rear) {
        return;
    }
    r_display(rear->next);
}

void CLL::r_display(Node *curr) {
    if(curr == rear) {
        return;
    }
    cout << "Curr: " << curr << endl;
    curr->print();
    r_display(curr->next);
    return;
}




