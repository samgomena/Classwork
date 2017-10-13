#include "fire_suppression.h"

int main() {
    cout << "In main." << endl;
    FireSuppression *areal = new Areal();
    FireSuppression *structural = new Structural();
    FireSuppression *wild = new Wild();
    
    areal->report();
    structural->report();
    wild->report();
    
    delete areal;
    delete structural;
    delete wild;
    
    return 0;
}