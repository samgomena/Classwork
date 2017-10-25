/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */
 
#include "utils.h"

class StudyMaterials {
public:
    virtual void display() const;
};

class Textbook : public StudyMaterials {
public:
    void display() const;
};

class Programming : public StudyMaterials {
public:
    Programming();
    ~Programming();
    Programming(const Programming &);
    void display() const;
};

class Slides : public StudyMaterials {
public:
    void display() const;
};