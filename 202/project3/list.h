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
    float operator[](int);
    const float operator[](int) const;


protected:
    ListNode * head, * tail;

    void destroy(ListNode *&);
    bool copy(ListNode *&, ListNode *);
    bool add(ListNode *&);
    int display(ListNode *);
    float get_element_at(ListNode*, int, int) const;
};

#endif