/**
 * @file fire_suppression.cpp
 * @brief Implementation of doubly linked list and linearly linked list classes, respectively.
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "fire_suppression.h"

FireSuppression::FireSuppression() : MAX_FIRE_FIGHTERS(generate_random(5, 20)),
                                     MAX_WATER_AVAILABLE(generate_random(1000, 1000000)) {}
FireSuppression::~FireSuppression() {}
void FireSuppression::suppress(int suppression) {}
void FireSuppression::report() {
    WARN(); 
    cout << "Using fire suppression report which should be dynamically bound to one of:"
            "\n\tAreal(...)\n\tStructural()\n\tWild()\nclass types." << endl;
}

Areal::Areal() {
    if(DEBUG) {
        NOTICE();
        cout << "In areal default constructor." << endl;
    }
}
Areal::~Areal() {
    if(DEBUG) {
        NOTICE();
        cout << "In areal destructor." << endl;
    }
}
void Areal::report() {
    cout << "Areal Report." << endl;
}

Structural::Structural() {
    if(DEBUG) {
        NOTICE();
        cout << "In structural default constructor." << endl;
    }
}
Structural::~Structural() {
    if(DEBUG) {
        NOTICE();
        cout << "In structural destructor." << endl;
    }
}
void Structural::report() {
    cout << "Structural Report." << endl;
}

Wild::Wild() {
    if(DEBUG) {
        NOTICE();
        cout << "In wild default constructor." << endl;
    }
}
Wild::~Wild() {
    if(DEBUG) {
        NOTICE();
        cout << "In wild destructor." << endl;
    }
}
void Wild::report() {
    cout << "Wild Report." << endl;
}

