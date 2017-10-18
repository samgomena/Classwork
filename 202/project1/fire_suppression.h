/**
 * @file fire_suppression.h
 * @brief Implementation of fire suppression class and the three classes of the types of fire suppression supported.
 *
 * The subclasses in this hierarchy are meant to be consumed by the fire_manager class, once it has determined
 * there is a fire underway. The three different type of fire suppression techniques, modelled by the three
 * classes below correspond to the three different types of settings that a location is initialized with.
 * They are:
 *      Urban
 *      Suburban
 *      Wild
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#ifndef FIRE_SUPPRESSION_H
#define FIRE_SUPPRESSION_H

#include <iostream>

using namespace std;

class FireSuppression {
    public:
        FireSuppression();
        virtual ~FireSuppression();
        virtual void suppress(int);
        virtual void report();
    protected:
        void determine_effectiveness();
        const int MAX_FIRE_FIGHTERS;
        const int MAX_WATER_AVAILABLE;
        bool is_water_available;
        int max_water;
        
        int fire_fighters_available;
        int effectivness;
        
};

class UrbanSupport : public FireSuppression {
    public:
        UrbanSupport();
        ~UrbanSupport();
        void report();
};

class SuburbanSupport : public FireSuppression {
    public:
        SuburbanSupport();
        ~SuburbanSupport();
        void report();
};

class WildSupport : public FireSuppression {
    public:
        WildSupport();
        ~WildSupport();
        void report();
};
#endif


