#ifndef LIST_H
#define LIST_H

#include "utils.h"

class ListNode {
public:
    ListNode();
    ListNode(float);
    ~ListNode();
    ListNode(const ListNode&);
    ListNode& operator=(const ListNode&);
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

protected:
    ListNode * head, * tail;

    void destroy(ListNode *&);
    bool copy(ListNode *&, ListNode *);
    bool add(ListNode *&);
};

#endif