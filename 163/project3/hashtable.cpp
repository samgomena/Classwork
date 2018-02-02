#include <iostream>

#include "hashtable.h"

using namespace std;

HashTable::HashTable() : _size(0) {
    table = new Node *[TABLE_SIZE];

    for (int i = 0; i < TABLE_SIZE; i++)
        table[i] = new Node;
}

HashTable::~HashTable() {
    int i;
    for(i = 0; i < TABLE_SIZE; i++) {
        Node * head = table[i];
        Node * curr;
        while(head) {
            curr = head->getNext();
//            head->next = NULL; // This was here and I don't think it works properly
            head = nullptr;
            delete head;
            head = curr;
        }
    }
    //delete the array
    delete [] table;
}

void HashTable::insert(const char * key, const family aData) {
    //calculate the insertion position (the index of the array)
    unsigned long long idx = calculateIndex(key);
    //create a new node to hold data
    Node * newNode = new Node(aData);

    if (table[idx]) {
        newNode->setNext(table[idx]);
        table[idx] = newNode;
    } else {
        table[idx] = newNode;
    }
    _size++;
}

bool HashTable::retrieve(char const * const key, Node& aData) const {
    //calculate the retrieval position (the index of the array)
    unsigned long long idx = calculateIndex(key);

    //search for the data in the chain (linked list)
    Node * curr = table[idx];

    while(curr) {
        char *id = 0;
        if(strcmp(key, curr->getItem().getId()) == 0) {
            //find match and return the data
            aData = curr->getItem();
            return true;
        }
        else
            curr = curr->getNext();
    }
    //data is not in the table
    return false;
}

unsigned long long HashTable::calculateIndex(const char *const key) const {
    unsigned long long hash;
    int i;

    int length = (int) strlen(key);

    hash = (unsigned long long) key[0];

    // s[0]*32^n-1 + s[1]*32^n-2 + ... + s[n-1]
    for(i = 1; i < length; i++) {
        hash = (hash * 32 + key[i]);
    }
    return hash % TABLE_SIZE;
}

void HashTable::dumpOne(char const *const key) const {

    unsigned long long idx = calculateIndex(key);

    Node *curr = table[idx];
    while (curr->getNext()) {
        if(strcmp(key, curr->getItem().getId()) == 0) {
            cout << "Family ID: " << curr->getItem().getId() << endl;
            cout << "  Name: " << curr->getItem().getName() << "\n  Members: " << curr->getItem().getMembers() << "\n  Friends: ";
            curr->getItem().listFriends();
            break;
        } else {
            curr = curr->getNext();
        }
    }
}

void HashTable::dumpMates(const char *const key) const {

    unsigned long long idx = calculateIndex(key);
    int i;
    Node *curr = table[idx];
    while (curr->getNext()) {
        if(strcmp(key, curr->getItem().getId()) == 0) {
            cout << "== Friends (1 level) ==" << endl;
            for(i = 0; i < curr->getItem().getMembers(); i++) {
                dumpOne(curr->getItem().getFamListOfI(i));
            }
            break;
        } else {
            curr = curr->getNext();
        }
    }
}

void HashTable::dumpTable() const {
    int i;
    for (i = 0; i < TABLE_SIZE; i++) {
        cout << "table[" << i << "]:" << "\nList:\n";
        Node *curr = table[i];
        while (curr->getNext()) {
            cout << "Family ID: " << curr->getItem().getId() << endl;
            cout << "  Name: " << curr->getItem().getName() << "\n  Members: " << curr->getItem().getMembers() << "\n  Friends: ";
            curr->getItem().listFriends();
            curr = curr->getNext();
        }
        cout << "--------------------" << endl;
    }
}




