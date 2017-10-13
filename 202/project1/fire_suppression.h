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
        void determine_effectivness();
        const int MAX_FIRE_FIGHTERS;
        const int MAX_WATER_AVAILABLE;
        bool is_water_available;
        int max_water;
        
        int fire_fighters_available;
        int effectivness;
        
};

class Areal : public FireSuppression {
    public:
        Areal();
        ~Areal();
        void report();
};

class Structural : public FireSuppression {
    public:
        Structural();
        ~Structural();
        void report();
};

class Wild : public FireSuppression {
    public:
        Wild();
        ~Wild();
        void report();
};
#endif


