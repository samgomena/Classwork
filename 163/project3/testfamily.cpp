#include <iostream>
#include "family.h"

using namespace std;

void addFriendHelper(family& fam,const char* myfriend)
{
    if (!fam.addFriend(myfriend))
    {
	cout << "Too many friends for " << fam.getId() << endl;
    }
}

int main()
{
    // Test some of the basic family functionality. Normally a test like this
    // should be self-checking but for this class I am just having it print to
    // screen since I think that will be more helpful for you (the students)

    family fam("Test001","Test",3);
    cout << fam;


    addFriendHelper(fam,"Friend001");
    cout << fam;


    addFriendHelper(fam,"Friend002");
    cout << fam;

    addFriendHelper(fam,"Friend003");
    cout << fam;

    addFriendHelper(fam,"Friend004");
    cout << fam;

    return(0);
}
