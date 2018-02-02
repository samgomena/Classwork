#include <iostream>
#include <cmath>
#include "plant.h"
#include "planttree.h"

using namespace std;

void showResults(planttree pt)
{
    pt.display();

    const plant* bestPlant;

    cout << endl;

    cout << "Best growth rate: " << endl;
    bestPlant = pt.findBestGrowth();
    if (bestPlant != nullptr)
	cout << (*bestPlant) << endl;
    else
	cout << "ERROR: null best plant" << endl;

    cout << "Best nutritional value: " << endl;
    bestPlant = pt.findBestNutrition();
    if (bestPlant != nullptr)
	cout << (*bestPlant) << endl;
    else
	cout << "ERROR: null best plant" << endl;

    cout << "Best water requirement: " << endl;
    bestPlant = pt.findBestWater();
    if (bestPlant != nullptr)
	cout << (*bestPlant) << endl;
    else
	cout << "ERROR: null best plant" << endl;

}

plant getChildPlant(plant parent,int co[])
{
    int gx = parent.getGrowth() - 50;
    int nx = parent.getNutrition() - 50;
    int wx = parent.getWater() - 50;

    gx = abs(co[0] * (gx * gx * gx) + co[1] * ( gx * gx ) + co[2] * gx + co[3]);
    gx = gx % 100;

    nx = abs(co[4] * (nx * nx * nx) + co[5] * ( nx * nx ) + co[6] * nx + co[7]);
    nx = nx % 100;

    wx = abs(co[8] * (wx * wx * wx) + co[9] * ( wx * wx ) + co[10] * wx + co[11]);
    wx = wx % 100;

    char* plantId = new char[15]; // max size ==> Plant ??-??-??  == 15 chars
    sprintf(plantId,"Plant %d-%d-%d",gx,nx,wx);
    plant plant(plantId,gx,nx,wx);
    delete [] plantId;

    return(plant);
}

void runSingleExperiment(planttree& pt,int depth,plant& parentPlant)
{
    if (depth > 0)
    {
	int leftCoeffs[] = {13,3,11,7,2,23,5,29,3,37,11,13};
	int rightCoeffs[] = {128,16,64,2,32,8,2,128,256,16,16,64};
	plant leftPlant = getChildPlant(parentPlant,leftCoeffs);
	plant rightPlant = getChildPlant(parentPlant,rightCoeffs);

	pt.addChildren(parentPlant,leftPlant,rightPlant);

	runSingleExperiment(pt,depth-1,leftPlant);
	runSingleExperiment(pt,depth-1,rightPlant);
    }
}

void runExperiment(const char* title, plant startingPlant, int depth)
{
    planttree pt;    
    pt.setRoot(startingPlant);

    runSingleExperiment(pt,depth,startingPlant);

    cout << "===================================" << endl;
    cout << title << endl;
    cout << "===================================" << endl;
    showResults(pt);
    cout << endl;
    cout << endl;
}

int main()
{
    runExperiment("Experiment 1",plant("Plant 1-1-1",1,1,1),5);
    runExperiment("Experiment 2",plant("Plant 11-17-33",11,17,33),5);
    
    return(0);
}
