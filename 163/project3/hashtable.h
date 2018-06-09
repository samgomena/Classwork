#ifndef HASHTABLE_H
#define HASHTABLE_H

#include "family.h"
#include "node.h"

class HashTable {
private:
    static const int TABLE_SIZE = 7877; // This is prime
    Node **table;
    int _size;

    unsigned long long calculateIndex(const char *const key) const;

public:
    HashTable();
    ~HashTable();

    void insert(const char * key, const family fam_data);
    bool retrieve(char const * const key, Node& fam_data)const;
    void dumpOne(char const *const key) const;
    void dumpMates(char const *const key) const;
    void dumpTable() const;

    bool destroyCollisions(Node* curr);
};

#endif