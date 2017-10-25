/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */
 
#include "study_materials.h"


void StudyMaterials::display() const {
    cout << "Study Materials base class." << endl;
}



void Textbook::display() const {
    cout << "Textbook Display" << endl;
}

Programming::Programming() {}
Programming::~Programming() {}
Programming::Programming(const Programming &copy_me) {}
void Programming::display() const {
    cout << "Programming Display" << endl;
}

void Slides::display() const {
    cout << "Slides Display" << endl;
}





