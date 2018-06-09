#include <iostream>
#include <fstream>
#include <cstring>
#include <cstdlib>
#include "familymgr.h"

using namespace std;


int main(int argc,char** argv)
{
    if (argc != 2) 
    {
	cout << "Usage: " << argv[0] << " <datafile>" << endl;
	exit(0);
    }

    // family manager object;
    familymgr familyMgr;

    // Read the data
    const int MAX_LINE = 64;
    char* datafile = argv[1];
    ifstream infile(datafile);
    char line[MAX_LINE];
    char id[MAX_LINE];
    char name[MAX_LINE];
    int members;
    char friend1[MAX_LINE];
    char friend2[MAX_LINE];
    char friend3[MAX_LINE];

    if (infile.is_open())
    {
	while (infile.getline(line,MAX_LINE) )
	{
	    char* s;
	    // ID -- Family ID: <id>
	    s = strchr(line,':') + 2;  // Skip the space
	    strncpy(id,s,MAX_LINE);

	    // Name
	    infile.getline(line,MAX_LINE);
	    s = strchr(line,':') + 2;  // Skip the space
	    strncpy(name,s,MAX_LINE);
	    
	    // members
	    infile.getline(line,MAX_LINE);
	    s = strchr(line,':') + 2;  // Skip the space
	    members = atoi(s);

	    // friends
	    infile.getline(line,MAX_LINE);
	    s = strchr(line,':') + 2;  // Skip the space
	    
	    char* friendPtr;
	    friendPtr = strtok(s," ");
	    if (friendPtr != nullptr)
		strncpy(friend1,friendPtr,MAX_LINE);
	    else
		friend1[0] = '\0';

	    friendPtr = strtok(nullptr," ");
	    if (friendPtr != nullptr)
		strncpy(friend2,friendPtr,MAX_LINE);
	    else
		friend2[0] = '\0';

	    friendPtr = strtok(nullptr," ");
	    if (friendPtr != nullptr)
		strncpy(friend3,friendPtr,MAX_LINE);
	    else
		friend3[0] = '\0';
	    
	    infile.getline(line,MAX_LINE);
	    if (strcmp(line,"---")!=0) {
		cout << "Error parsing the file" << endl;
	    }

	    // Add the family to the family manager
	    family* famPtr = new family(id,name,members);
	    famPtr->addFriend(friend1);
	    famPtr->addFriend(friend2);
	    famPtr->addFriend(friend3);

	    familyMgr.addFamily(*famPtr);
	    delete famPtr;
	}
	infile.close();

//	familyMgr.printAllFamilies();
//	familyMgr.printGroup("Smith001");
//	familyMgr.printSmallCircle("Smith001");
//	familyMgr.printSmallCircle("Hall001");
	familyMgr.printSmallCircle("Noel003");
    }

    return(0);
}
