#include <iostream>

#include "familymgr.h"

using namespace std;

familymgr::familymgr() {
    hashTable = new HashTable();
}

familymgr::~familymgr() {
    delete hashTable;
}

void familymgr::addFamily(family fam) {
    const char *famId = fam.getId();
    hashTable->insert(famId, fam);
}

void familymgr::printAllFamilies() {
    hashTable->dumpTable();
}

void familymgr::printSmallCircle(const char * familyId) {
    Node familyData;

    if (hashTable->retrieve(familyId, familyData)) {

        const char *id = familyData.getItem().getId();
        cout << "Printing family and immediate friends " << id <<  "\n== Family ==" << endl;
        hashTable->dumpOne(familyId);
        hashTable->dumpMates(familyId);
    }
}


