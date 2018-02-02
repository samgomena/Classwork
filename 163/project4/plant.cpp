#include "plant.h"

#include <cstring>

using namespace std;

plant::plant(char *id, int growth_rate, int nutritional_value, int water_requirement) :
       _id(nullptr), _growth_rate(growth_rate), _nutritional_value(nutritional_value), _water_requirement(water_requirement) {
    // Make sure we don't deallocate uninitialized memory
    setId(id);
}
plant::~plant() {
    // TODO: not sure why this breaks everything but i'm getting kicked out of the building...
//    if(this->_id) {
//        delete [] _id;
//    }
}

const char *plant::getId() const {
    return _id;
}

int plant::getGrowth() const {
    return _growth_rate;
}

int plant::getNutrition() const {
    return _nutritional_value;
}

int plant::getWater() const {
    return _water_requirement;
}

void plant::setId(const char *id) {
    if(this->_id)
        delete [] _id;

    this->_id = new char[strlen(id)+1];
    strcpy(this->_id, id);
}

void plant::setGrowth(const int growth_rate) {
    _growth_rate = growth_rate;
}

void plant::setNutrition(const int nutritional_value) {
    _nutritional_value = nutritional_value;
}

void plant::setWater(const int water_requirement) {
    _water_requirement = water_requirement;
}

std::ostream& operator<<(std::ostream& os, const plant& pt) {
    // Plant ID: Plant \d\d-\d\d-\d\d (G: \d\d N: \d\d W: \d\d)
    os << "Plant ID: " << pt._id << " " << "(G: " << pt._growth_rate << " N: " << pt._nutritional_value <<
                                                                    " W: " << pt._water_requirement << ")";
    return os;
}
