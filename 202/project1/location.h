/**
 * Author: Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */
 
#ifndef LOCATION_H
#define LOCATION_H

#include <iostream>

#include "utils.h"

class Location {
    public:
        Location();
        ~Location();
        //Location(const Location &);
        
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
        //City(const City &);
        City(int);
        const int id() const;
        friend ostream& operator<<(ostream& steam, const City&);
    protected:
        int city_id;
        bool on_fire;
        bool is_on_fire();
        void is_on_fire(bool);
};


#endif
