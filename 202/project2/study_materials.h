/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef STUDY_MATERIALS_H
#define STUDY_MATERIALS_H

#include "utils.h"

class StudyMaterials {

public:
    StudyMaterials();
    virtual ~StudyMaterials();
    virtual void display() const;
    virtual const char * data() const;
};

class Textbook : public StudyMaterials {
public:
    Textbook();
    ~Textbook();
    Textbook(const Textbook&);
    void display() const;
    const char * data() const;

protected:
    const char * textbook_review;
};

class Programming_Node {
public:
    Programming_Node(int);
//    ~Programming_Node();
    Programming_Node(const Programming_Node &);
    Programming_Node *next;

    const char *question() const;
    const char *answer() const;
    const char * data() const;

protected:
    const char * _question;
    const char * _answer;
};

class Programming : public StudyMaterials {
public:
    Programming();
    ~Programming();
    Programming(const Programming &);
    void display() const;
    Programming_Node *head;
    Programming_Node *tail;

    const char * data() const;
    bool add(Programming_Node *&);
    void destroy(Programming_Node *&);
    void copy(Programming_Node *&, Programming_Node *);
    void r_display(Programming_Node *) const;
};



class Slides : public StudyMaterials {
public:
    Slides();
//    ~Slides();
//    Slides(const Slides &);
    void display() const;
    const char * data() const;

protected:
    const char * slide_review;
};

#endif