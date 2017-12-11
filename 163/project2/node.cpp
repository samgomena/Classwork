/**
 * @file node.cpp
 */

#include "node.h"

template <class T>
Node<T>::Node() : next( nullptr ) {}

template <class T>
Node<T>::Node(const T& x, const T& y) : _x(x), _y(y), next(nullptr) {}

template <class T>
Node<T>::Node(const T& data, Node<T>* nextNodePtr) : _scanData(data), next(nextNodePtr) {}

template <class T>
void Node<T>::setCoords(const T& x, const T& y) {
    this->_x = x;
    this->_y = y;
}

template <class T>
void Node<T>::setNext(Node<T>* nextNodePtr) {
    next = nextNodePtr;
}

template <class T>
T Node<T>::getCoords() const {
    return _x, _y;
}

template <class T>
Node<T>* Node<T>::getNext() const {
    return next;
}