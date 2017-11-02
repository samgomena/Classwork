/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */
 
#include "study_materials.h"

StudyMaterials::StudyMaterials() {}
StudyMaterials::~StudyMaterials() {}

void StudyMaterials::display() const {
    cout << "Study Materials display method." << endl;
}

const char * StudyMaterials::data() const {
    cout << "Study Materials data method." << endl;
}

// -----------------------------------------------------
// Textbook Questions
// -----------------------------------------------------
Textbook::Textbook() : textbook_review(TEXTBOOK_ARRAY[generate_random(0, 3)]) {}

Textbook::~Textbook() {}

void Textbook::display() const {
    cout << textbook_review << endl;
}

const char * Textbook::data() const {
    return textbook_review;
}

Programming_Node::Programming_Node(int qa_idx) :
        next(NULL),
        _question(QUESTION_ARRAY[qa_idx]),
        _answer(ANSWER_ARRAY[qa_idx]) {}

Programming_Node::Programming_Node(const Programming_Node & copy_me) :
        next(NULL),
        _question(copy_me._question),
        _answer(copy_me._answer) {}

const char *Programming_Node::question() const {
    return _question;
}

const char *Programming_Node::answer() const {
    return _answer;
}

const char * Programming_Node::data() const {
    return _question;
};

// -----------------------------------------------------
// Programming Questions
// -----------------------------------------------------
Programming::Programming() : head(NULL), tail(NULL) {
    int qa_idx = 0;
    for(int i = 0; i < 6; ++i) {
        qa_idx = generate_random(0, 6);
        Programming_Node *new_node = new Programming_Node(qa_idx);
        add(new_node);
    }
}
Programming::~Programming() {
    destroy(head);
}
Programming::Programming(const Programming &copy_me) {
    copy(head, copy_me.head);
}

void Programming::display() const {
    r_display(head);
}

const char * Programming::data() const {
    if(head) {
        return head->data();
    }

}

void Programming::r_display(Programming_Node *curr) const {
    if(!curr) {
        return;
    }
    cout << "Question: " << curr->question() << "\nAnswer: " << curr->answer() << endl;
    r_display(curr->next);
    return;
}

void Programming::destroy(Programming_Node *& head) {
    if(!head) {
        return;
    }
    destroy(head->next);
    delete head;
    head = NULL;
}

bool Programming::add(Programming_Node *& new_node) {
    // Push front
    if(!head) {
        head = new_node;
        tail = new_node;
        return true;
    } else {
        new_node->next = head;
        head = new_node;
        return true;
    }
}

void Programming::copy(Programming_Node *& dest, Programming_Node * src) {
    if(!src) {
        dest = NULL;
        return;
    }
    dest = new Programming_Node(*src);
    copy(dest->next, src->next);
    return;
}

// -----------------------------------------------------
// Slide Questions
// -----------------------------------------------------

Slides::Slides() : slide_review(SLIDE_ARRAY[generate_random(0, 9)]) {}

void Slides::display() const {
    cout << slide_review << endl;
}

const char * Slides::data() const {
    return slide_review;
}





