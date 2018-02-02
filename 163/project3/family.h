#ifndef FAMILY_H
#define FAMILY_H

#include <cstring>

class family {
private:
    char * _id;
    char * _name;
    int _num_fam_mems;
    char *_fam_list[3] = {nullptr, nullptr, nullptr};

public:
    family();
    family(char *, char *, int);
    ~family();

    const char *getId() const;
    const char *getName() const;
    int getMembers() const;
    const char * getFamListOfI(int) const;
    void listFriends() const;

    void addFriend(char []);

    void setId(char *name);

    void setName(char *name);
};

#endif

