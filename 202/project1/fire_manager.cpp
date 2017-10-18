/**
 * @file fire_manager.cpp
 * @brief Implementation of the `Fire` and `FireManager` classes respectively.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "fire_manager.h"

Fire::Fire() : size((float) generate_random(100, 2000)), severity(generate_random(1, 4)) {
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

FireManager::FireManager(int setting) {
    if(DEBUG) {
        NOTICE();
        cout << "Inside fire manager constructor." << endl;
    }
    manager_list = new DLL();
    if(setting == 1) {
        fs = new UrbanSupport();
    } else if(setting == 2) {
        fs = new SuburbanSupport();
    } else {
        fs = new WildSupport();
    }
    manager_list->add(fs);
}

FireManager::~FireManager() {
    if(DEBUG) {
        NOTICE();
        cout << "Inside fire manager destructor." << endl;
    }
    delete fs;
    delete manager_list;
}

FireManager::FireManager(const FireManager & to_copy) : fs(to_copy.fs), manager_list(to_copy.manager_list) {}