#include <iostream>

#include "family.h"

using namespace std;

family::family() : _id(nullptr), _name(nullptr), _num_fam_mems(0) {}

family::family(char* id, char* name, int members) : _id(nullptr), _name(nullptr), _num_fam_mems(members) {
    setName(name);
    setId(id);
}

family::~family() {
    delete [] _id;
    _id = nullptr;
    delete [] _name;
    _name = nullptr;
    for(int i = 0; i < 3; ++i) {
        if(_fam_list[i]) {
            delete [] _fam_list[i];
            _fam_list[i] = nullptr;
        }
    }
}

family::family(const family& copy_fam) : _id(nullptr), _name(nullptr), _num_fam_mems(copy_fam._num_fam_mems) {
    setName(copy_fam._name);
    setId(copy_fam._id);
    for(int i = 0; i < 3; ++i) {
        if(copy_fam._fam_list[i]) {
            _fam_list[i] = new char[strlen(copy_fam._fam_list[i])+1];
            strcpy(_fam_list[i], copy_fam._fam_list[i]);
        }
    }
}

family& family::operator=(const family& copy_fam) {
    if(this == &copy_fam) {
        return *this;
    }
    setId(copy_fam._id);
    setName(copy_fam._name);
    this->_num_fam_mems = copy_fam._num_fam_mems;

    for(int i = 0; i < 3; ++i) {
        if(copy_fam._fam_list[i]) {
            // Delete old data
            if(this->_fam_list[i]) {
                delete [] _fam_list[i];
            }
            // Set new data
            this->_fam_list[i] = new char[strlen(copy_fam._fam_list[i])+1];
            strcpy(this->_fam_list[i], copy_fam._fam_list[i]);
        }
    }
    return *this;
}

void family::addFriend(char* friend_name) {
    int i = 0;
    while(i < 3) {
        if(!_fam_list[i]) {
            _fam_list[i] = new char[strlen(friend_name)+1];
            strcpy(_fam_list[i], friend_name);
            break;
        }
        ++i;
    }
}

void family::setId(char * id) {
    if(_id) {
        delete[] _id;
    }
    _id = new char[strlen(id)+1];
    strcpy(_id, id);
}

void family::setName(char * name) {
    if(_name) {
        delete [] _name;
    }
    _name = new char[strlen(name)+1];
    strcpy(_name, name);
}

const char* family::getId() const {
    return _id;
}

const char* family::getName() const {
    return _name;
}

int family::getMembers() const {
    return _num_fam_mems;
}

void family::listFriends() const {
    for(int i = 0; i < 3; i++) {
        if(_fam_list[i])
        i != 2 ? cout << _fam_list[i] << " " :  cout << _fam_list[i];
    }
    cout << endl;

}

const char *family::getFamListOfI(int famIdx) const {
    return _fam_list[famIdx];
}
