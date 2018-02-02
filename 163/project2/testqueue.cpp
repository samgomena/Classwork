#include <iostream>
#include "queue.h"

using namespace std;

void dumpQueue(queue &q)
{
    cout << "Dumping the queue" << endl;
    while (!q.isEmpty())
    {
	int result = q.dequeue();
	cout << "Value: " << result << endl;
    }
}

int main()
{
    queue q(5);

    q.enqueue(1);
    dumpQueue(q);

    q.enqueue(1);
    q.enqueue(2);
//    q.printInternals();
    dumpQueue(q);

    q.enqueue(1);
    q.enqueue(2);
    q.enqueue(3);
    q.enqueue(4);
    q.enqueue(5);
    q.enqueue(6);
//    q.printInternals();
    dumpQueue(q);

}
