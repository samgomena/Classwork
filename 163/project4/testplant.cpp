#include <iostream>

#include "plant.h"

using namespace std;

void doSomething(plant p1)
{
    cout << p1;
}

int main()
{
    plant p1("001",1,1,1);
    doSomething(p1);

    return(0);
}
