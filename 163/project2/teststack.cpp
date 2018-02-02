#include <iostream>
#include "stack.h"

using namespace std;

void dumpStack(stack &s)
{
    cout << "Dumping the stack" << endl;
    while (!s.isEmpty())
    {
	const stack_entry* result = s.pop();
	cout << "Value: " << result->x << ", " << result->y << endl;
    }
}

void pushHelper(stack &s,int x, int y)
{
    stack_entry entry;

    entry.x = x;
    entry.y = y;
    s.push(entry);
}

int main()
{
    stack s(5);
    stack_entry entry;

    pushHelper(s,1,1);
    dumpStack(s);

    pushHelper(s,1,1);
    pushHelper(s,2,2);
    dumpStack(s);

}
