#ifndef FIRE_H
#define FIRE_H

#include "fire_suppression.h"

class Fire {
    public:
        Fire();
        ~Fire();
        float get_size();
        int get_severity();
        void update_size(float);
        void update_severity(int);
    protected:
        float size;
        int severity;
};

class FireManager : public Fire {
    public:
        FireManager();
        ~FireManager();
        float get_size();
    protected:
        FireSuppression *fs;
        void determine_suppression(int, int);
};

#endif
