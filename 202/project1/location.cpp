/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "location.h"

// ----------------------------------------------------------
// LOCATION
// ----------------------------------------------------------
int Location::id_generator = 0;
Location::Location(): id(id_generator++),
                      population_size(generate_random(MIN_POP_SIZE, MAX_POP_SIZE)), 
                      area(150000.0),
                      setting(generate_random(1, 3)) {
    if(DEBUG) {
        NOTICE();
        cout << "Location" << id << " constructor called." << endl;
    }          
}
Location::~Location(){
    if(DEBUG) {
        NOTICE();
        cout << "Location" << id << " destructor called." << endl;
    }
}
// Location::Location(const Location &);

// ----------------------------------------------------------
// CITY
// ----------------------------------------------------------

City::City() : on_fire(false) {}
City::City(bool fire_bool) : on_fire(fire_bool) {}
City::~City() {}
// City::City(const City &){}

bool City::is_on_fire() {
    return on_fire;
}

void is_on_fire(bool fire_bool) {
    on_fire = fire_bool;
}



