#ifndef _PLANT
#define _PLANT

#include <iostream>

class plant {
private:
    char *_id;
    int _growth_rate;
    int _nutritional_value;
    int _water_requirement;
public:
    plant(char *, int, int, int);
    ~plant();

    const char *getId() const;
    int getGrowth() const;
    int getNutrition() const;
    int getWater() const;

    friend std::ostream& operator<<(std::ostream& os, const plant& pt);
    
    void setId(const char *);
    void setGrowth(const int);
    void setNutrition(const int);
    void setWater(const int);
};
#endif