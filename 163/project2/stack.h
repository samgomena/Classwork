#ifndef STACK_H
#define STACK_H

class Stack {
public:
    Stack();
    ~Stack();
    bool isEmpty() const;
    bool push(const int x, const int y);
    bool pop();
    int peekX() const;
    int peekY() const;
private:
    class Node {
        int _x;
        int _y;
    public:
        Node *next;
        Node(int x, int y) {
            _x = x;
            _y = y;
        }
        int getX() { return _x; }
        int getY() { return _y; }
    };
    Node *top;
};

#endif