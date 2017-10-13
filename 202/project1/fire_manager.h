#ifndef FIRE_H
#define FIRE_H

#include "fire_suppression.h"

class Fire {
    public:
        Fire();
        ~Fire();
        int get_size();
        int get_severity();
    protected:
        float size;
        int severity;
};

class FireManager : public Fire {
    public:
        FireManager();
        ~FireManager();
    protected:
        FireSuppression *fs;
        void determine_suppression(int, int);
};

#endif