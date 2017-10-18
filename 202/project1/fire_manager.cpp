/**
 * @file fire_manager.cpp
 * @brief Implementation of the `Fire` and `FireManager` classes respectively.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "utils.h"
#include "fire_manager.h"


Fire::Fire() {
    if(DEBUG) {
        NOTICE();
        cout << "Inside fire constructor." << endl;
    }
}

Fire::~Fire() {
    if(DEBUG) {
        NOTICE();
        cout << "Inside Fire destructor." << endl;
    }
}

float Fire::get_size() {
    return size;
}

int Fire::get_severity() {
    return severity;
}

void Fire::update_size(float new_size) {
    size = new_size;
}

void Fire::update_severity(int new_severity) {
    severity = new_severity;
}

FireManager::FireManager() {
    if(DEBUG) {
        NOTICE();
        cout << "Inside fire manager constructor." << endl;
    }  
}

FireManager::~FireManager() {
    if(DEBUG) {
        NOTICE();
        cout << "Inside fire manager destructor." << endl;
    }
}
