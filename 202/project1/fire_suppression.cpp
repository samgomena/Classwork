/**
 * @file fire_suppression.cpp
 * @brief Implementation of fire suppression base class and three fire suppression subclasses each pertaining
 * to a specific fire fighting technique.
 *
 * @author Sam Gomena
 * Class: CS202 Fall 2017
 * Instructor: Karla Fant
 */

#include "fire_suppression.h"

FireSuppression::FireSuppression() : MAX_FIRE_FIGHTERS(generate_random(5, 20)),
                                     MAX_WATER_AVAILABLE(generate_random(1000, 1000000)) {}
FireSuppression::~FireSuppression() {}
void FireSuppression::suppress(int suppression) {}
void FireSuppression::report() {
    WARN(); 
    cout << "Using fire suppression report which should be dynamically bound to one of:"
            "\n\tAreal(...)\n\tStructural()\n\tWild()\nclass types." << endl;
}

UrbanSupport::UrbanSupport() {
    if(DEBUG) {
        NOTICE();
        cout << "In urban default constructor." << endl;
    }
}
UrbanSupport::~UrbanSupport() {
    if(DEBUG) {
        NOTICE();
        cout << "In urban destructor." << endl;
    }
}
/**
 * @brief Report back the state of this techniques effectiveness fighting the fire.
 *
 * @return void
 */
void UrbanSupport::report() {
    cout << "Urban Report." << endl;
}

SuburbanSupport::SuburbanSupport() {
    if(DEBUG) {
        NOTICE();
        cout << "In SuburbanSupport default constructor." << endl;
    }
}
SuburbanSupport::~SuburbanSupport() {
    if(DEBUG) {
        NOTICE();
        cout << "In SuburbanSupport destructor." << endl;
    }
}
/**
 * @brief Report back the state of this techniques effectiveness fighting the fire.
 *
 * @return void
 */
void SuburbanSupport::report() {
    cout << "Suburban Report." << endl;
}

WildSupport::WildSupport() {
    if(DEBUG) {
        NOTICE();
        cout << "In wild default constructor." << endl;
    }
}
WildSupport::~WildSupport() {
    if(DEBUG) {
        NOTICE();
        cout << "In wild destructor." << endl;
    }
}
/**
 * @brief Report back the state of this techniques effectiveness fighting the fire.
 *
 * @return void
 */
void WildSupport::report() {
    cout << "Wild Report." << endl;
}

