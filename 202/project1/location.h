/**
 * @file location.h
 * @brief Header file for class and method definitions of Location and City classes.
 *
 * `City` is the data member of the nodes in the linked-list. The `City` class maintains
 * information pertaining to the state of the city. That is, which city it is and whether its on fire.
 *
 * The `Location` class maintains information about the - you guessed it - the location.
 * That is, the population, the land area, and the type of area it's in. All these values are initialized
 * randomly using the `generate_random(...)` function in `utils.cpp`.
 *
 * @author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */
 
#ifndef LOCATION_H
#define LOCATION_H

//#include <iostream>
#include "utils.h"
#include "fire_manager.h"


class Location {
    public:
        Location();
        ~Location();

    protected:
        int population_size;
        float area;
        int setting;
        
    private:
        // 1,000,000
        const static int MAX_POP_SIZE = 1000000;
        // 20,000
        const static int MIN_POP_SIZE = 20000;
};

class City : public Location {
    public:
        City();
        ~City();
        City(int);
        const int id() const;
        friend ostream& operator<<(ostream&, const City&);
    protected:
        int city_id;
        bool on_fire;
        FireManager *fm;
        bool is_on_fire();
        void is_on_fire(bool);
};

#endif
