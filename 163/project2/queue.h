#ifndef QUEUE_H
#define QUEUE_H

class Queue {
public :
    Queue();
    ~Queue();
    bool isEmpty() const;
    bool enqueue(const int);
    bool dequeue();
    int peek() const;

private :
    class Node {
        int _data;
    public:
        Node *next;
        Node(int data) : _data(data) {};
        int getData() {return _data;}
    };

    Node *front, *back;
};
#endif