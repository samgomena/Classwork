/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "location.h"

// ----------------------------------------------------------
// LOCATION
// ----------------------------------------------------------
Location::Location(): population_size(generate_random(MIN_POP_SIZE, MAX_POP_SIZE)), 
                      area(generate_random(50, 1000)),
                      setting(generate_random(1, 3)) {
    if(DEBUG) {
        NOTICE();
        cout << "Location constructor called." << endl;
    }          
}
Location::~Location(){
    if(DEBUG) {
        NOTICE();
        cout << "Location  destructor called." << endl;
    }
}
// Location::Location(const Location &);

// ----------------------------------------------------------
// CITY
// ----------------------------------------------------------

City::City() : on_fire(false) {}
City::City(int id) : on_fire(false), city_id(id){}
City::~City() {}
// City::City(const City &){}

int City::id() {
    return city_id;
}

bool City::is_on_fire() {
    return on_fire;
}

void City::is_on_fire(bool fire_bool) {
    on_fire = fire_bool;
}


ostream& operator<<(ostream& os, const City& city) {
    os << "Pop: " << city.population_size << "\nArea: " << city.area << "mi^2\nSetting: " << city.setting << endl << endl;
    return os;
}

