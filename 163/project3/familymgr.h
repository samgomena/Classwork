#ifndef FAMILYMGR_H
#define FAMILYMGR_H

#include "family.h"
#include "hashtable.h"

class familymgr {
private:
    HashTable *hashTable;

public:
    familymgr();

    ~familymgr();

    void addFamily(family);

    void printAllFamilies();

    void printSmallCircle(const char []);
};

#endif
