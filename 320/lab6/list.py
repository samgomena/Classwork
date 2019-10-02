class List(object):

    def __str__(self):
        return f"[{self.comma_elements()}]"

    def length(self):
        return self.lengthAcc(0)

    def sum(self):
        return self.sumAcc(0)

    def reverse(self):
        return self.revOnto(Nil())

class Nil(List):
    """ Represents an empty list. """

    def comma_cons(self, s):
        return s
    
    def length(self):
        """ There are no elements in an empty list. """
        return 0

    def zip(self, other):
        return Nil()

    def __add__(self, ys):
        return ys

    def map(self, f):
        return self

    def filter(self, f):
        return Nil()
    
    def sumAcc(self, n):
        return n
    
    def revOnto(self, ys):
        return ys

class Cons(List):
    """ Represents a non-empty list. """
    def __init__(self, head, tail):
        # super(self).__init__()
        self.head = head
        self.tail = tail

    def comma_elements(self):
        return self.tail.comma_cons(str(self.head))

    def comma_cons(self, s):
        return "{}, {}".format(s, self.comma_elements())

    def length(self):
        """ Count the elements in this list.  """
        return 1 + self.tail.length()

    def zip(self, other):
        return Cons(self, self.tail.zip_n(other.tail))
        # return Cons((self, other), self.tail.zip_n(other))

    def zip_n(self, other):
        return Cons((self.head, other.head), self.tail.zip(other.tail))

    def __add__(self, ys):
        return Cons(self.head, self.tail + ys)

    def map(self, f):
        return Cons(f(self.head), self.tail.map(f))

    def filter(self, b):
        return Cons(self.head, self.tail.filter(b)) if b(self.head) else self.tail.filter(b)

    def sumAcc(self, n):
        return self.head + self.tail.sumAcc(n)

    def revOnto(self, ys):
        return self.tail.revOnto(Cons(self.head, ys))

alist = Cons(1, Cons(2, Cons(3, Nil())))
blist = Cons(4, Cons(5, Cons(6, Cons(7, Nil()))))

#print(alist)
#print(blist)

#print(alist + blist)
#print(blist + alist)

#print(alist.length())
#print(blist.length())


def square(x):
    return x * x

# print(alist.map(square))
# print(alist.map(lambda x: x+1))

def isEven(x):
    return (x % 2) == 0

# print(alist.filter(isEven))
# print(alist.filter(lambda x: not(isEven(x))))
#
# print(alist.sumAcc(0))
# print(alist.sumAcc(4))
#
# print(alist.revOnto(Cons(1, Nil())))
# print(alist.revOnto(blist))
#
# print(blist.reverse())

def nums(lo, hi):
    return Cons(lo, nums(lo+1, hi)) if lo < hi else Nil()

# print(nums(0,5))
print(nums(0, 6), nums(1, 7))
print(nums(0,6).zip(nums(1,7)))
print(f"\n---\n")

print(nums(0,6), nums(0,3))
print(nums(0,6).zip(nums(0,3)))
print(f"\n---\n")

print(nums(0,3), nums(0,6))
print(nums(0,3).zip(nums(0,6)))


# def powers2(n):
#     tail = powers2(n-1) if n > 1 else Nil()
#     return Cons(2**n, tail)
#
# print(powers2(5))


