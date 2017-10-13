#include "fire_suppression.h"

// TODO: put in utils
const bool DEBUG = true;
void NOTICE() {
    cout << "NOTICE: ";
}
void WARN() {
    cout << "WARNING: ";
}

FireSuppression::FireSuppression() {}
FireSuppression::~FireSuppression() {}
void FireSuppression::suppress(int suppression) {}
void FireSuppression::report() {
    WARN(); 
    cout << "Using fire suppression report." << endl;
    
}

Areal::Areal() {
    if(DEBUG) {
        NOTICE();
        cout << "In areal default constructor." << endl;
    }
}
Areal::~Areal() {
    if(DEBUG) {
        NOTICE();
        cout << "In areal destructor." << endl;
    }
}
void Areal::report() {
    cout << "Areal Report." << endl;
}

Structural::Structural() {
    if(DEBUG) {
        NOTICE();
        cout << "In structural default constructor." << endl;
    }
}
Structural::~Structural() {
    if(DEBUG) {
        NOTICE();
        cout << "In structural destructor." << endl;
    }
}
void Structural::report() {
    cout << "Structural Report." << endl;
}

Wild::Wild() {
    if(DEBUG) {
        NOTICE();
        cout << "In wild default constructor." << endl;
    }
}
Wild::~Wild() {
    if(DEBUG) {
        NOTICE();
        cout << "In wild destructor." << endl;
    }
}
void Wild::report() {
    cout << "Wild Report." << endl;
}

