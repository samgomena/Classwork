#include <iostream>

#include "family.h"

using namespace std;

family::family() : _id(nullptr), _name(nullptr), _num_fam_mems(0) {}

family::family(char *id, char *name, int members) : _id(nullptr), _name(nullptr) {
    setName(name);
    setId(id);
    _num_fam_mems = members;
}

family::~family() {}

void family::addFriend(char * _friend) {
    int i = 0;
    while(i < 3) {
        if(_fam_list[i] == nullptr) {
            this->_fam_list[i] = new char[strlen(_friend)+1];
            strcpy(this->_fam_list[i], _friend);
            break;
        }
        i++;
    }
}

void family::setId(char * id) {
    //release the exisisting memory if there is any
    if(this->_id)
        delete [] this->_id;

    //set new name
    this->_id = new char[strlen(id)+1];
    strcpy(this->_id, id);
}

void family::setName(char * name) {
    //release the exisisting memory if there is any
    if(this->_name)
        delete [] this->_name;

    this->_name = new char[strlen(name)+1];
    strcpy(this->_name, name);
}

const char *family::getId() const {
    return _id;
}

const char *family::getName() const {
//    strcpy(name, this->_name);
    return _name;
}

int family::getMembers() const {
    return _num_fam_mems;
}

void family::listFriends() const {
    int i;
    for(i = 0; i < 3; i++) {
        if(_fam_list[i] != NULL)
        i != 2 ? cout << _fam_list[i] << " " :  cout << _fam_list[i];
    }
    cout << endl;

}

const char *family::getFamListOfI(int famIdx) const {
    return _fam_list[famIdx];
}
