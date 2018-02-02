#include <iostream>
#include "hashtable.h"

using namespace std;

int main()
{
    const int HASHTABLESIZE = 13;
    const int NUMFAMILIES = 50;

    hashtable ht(HASHTABLESIZE);

    cout << "======================================================================" << endl;
    cout << "Testing inserts (should show full table)" << endl;

    for (int i=0;i<NUMFAMILIES;i++)
    {
	char id[8];
	char name[8];
	char friendName[10];
	family* familyPtr;

	sprintf(id,"Test%d",i);
	sprintf(name,"Name%d",i);
	sprintf(friendName,"Friend%d",i);

	familyPtr = new family(id,name,1);
	familyPtr->addFriend(friendName);

	ht.insert(id,*familyPtr);

	delete familyPtr;
    }
    ht.dumpTable();

    cout << "======================================================================" << endl;
    cout << "Testing searches (should show no errors)" << endl;

    const family* foundFam;
    foundFam = ht.lookup("Test44");
    if (foundFam == nullptr)
	cout << "ERROR searching for Test44" << endl;
    foundFam = ht.lookup("Test39");
    if (foundFam == nullptr)
	cout << "ERROR searching for Test39" << endl;
    foundFam = ht.lookup("Test999");
    if (foundFam != nullptr)
	cout << "ERROR searching for Test999" << endl;

    cout << "======================================================================" << endl;
    cout << "Testing removes (should show empty table)" << endl;

    for (int i=0;i<NUMFAMILIES;i++)
    {
	char id[8];
	sprintf(id,"Test%d",i);
	ht.remove(id);
    }
    ht.dumpTable();

    return(0);
}
