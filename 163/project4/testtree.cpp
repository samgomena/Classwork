#include <iostream>

#include "planttree.h"

using namespace std;

void testCopyConstructor(planttree pt)
{
    cout << "-- Duplicate tree -- " << endl;
    pt.display();
}

int main()
{
    
    planttree* pt = new planttree();

    plant p1("001",1,1,1);
    plant p2("002",2,2,2);
    plant p3("003",3,4,5);
    pt->setRoot(p1);
    pt->addChildren(p1,p2,p3);

    plant p4("004",17,18,19);
    plant p5("005",25,22,20);
    plant p6("006",30,50,40);
    plant p7("007",33,44,55);

    pt->addChildren(p2,p4,p5);
    pt->addChildren(p3,p6,p7);

    pt->display();

    const plant* bestPlant;

    cout << "Best growth rate: " << endl;
    bestPlant = pt->findBestGrowth();
    cout << (*bestPlant) << endl;

    cout << "Best nutritional value: " << endl;
    bestPlant = pt->findBestNutrition();
    cout << (*bestPlant) << endl;

    cout << "Best water requirement: " << endl;
    bestPlant = pt->findBestWater();
    cout << (*bestPlant) << endl;

    testCopyConstructor(*pt);

    delete pt;

    return(0);
}
