#ifndef LIST_H
#define LIST_H

#include "utils.h"

class ListNode {
public:
    ListNode();
    ListNode(float);
    friend ostream& operator << (ostream&, const ListNode&);
    float data();
    ListNode *next;


protected:
    float data_point;
};

class Sol {
public:
    Sol();
    ~Sol();
    Sol(const Sol &);
    Sol& operator=(const Sol&);
    bool add(float);
    int display();


protected:
    ListNode * head, * tail;

    void destroy(ListNode *&);
    bool copy(ListNode *&, ListNode *);
    bool add(ListNode *&);
    int display(ListNode *);
};

#endif