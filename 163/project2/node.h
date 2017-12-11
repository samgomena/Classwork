/**
 * @file node.h
 */

#ifndef NODE_H
#define NODE_H

// Use template so can apply to Stack and Queue
template <class T>
class Node
{
private :
    T _x;
    T _y;
    T _scanData;
    Node<T>* next;
public :
    Node();
    Node(const T& x, const T& y);
    Node(const T& anItem, Node<T>* nextNodePtr);
    void setCoords(const T& x, const T& y);
    void setData(const T& data);
    void setNext(Node<T>* nextNodePtr);
    T getCoords() const ;
    Node<T>* getNext() const ;
};

#endif