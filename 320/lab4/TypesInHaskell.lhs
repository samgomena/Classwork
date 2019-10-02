-----------------------------------------------------------------------------
CS 320 Principles of Programming Languages                        Spring 2019
Lab 4

Andrew Tolmach and Mark Jones
-----------------------------------------------------------------------------
Goals for this lab:

* Build familiarity with different kinds of types (primitives, sums,
  products, functions, recursive types, parameterized types) and the
  operations that we use to manipulate their values.
  
* Build experience in writing a broader range of simple functional
  programs in Haskell.

-----------------------------------------------------------------------------
TYPES IN HASKELL: An Introduction

This is literate Haskell source file, suitable for loading in to
a Haskell interpreter like Hugs or ghci.

Only lines beginning with a '>' symbol in the first column are
treated as lines of code.  All other lines are considered to be
comments.  Note that there must be at least one blank line between
code lines and comment lines.

Haskell also provides several other methods for introducing comments
in code.  Including one line comments:

> -- This is a one-line comment, because it follows dash-dash

And bracketed comments, which can potentially span multiple lines:

> {- This is also a comment, because 
>    it is enclosed by curly-dash and dash-curly 
>    {- incidentally, these nest -} -}

This file contains explanatory text and a series of
exercises. Your work for this week is to read through the text
and to complete the exercises by adding additional code to this
file at the indicated places. To ensure that you receive credit
for your answers, do not remove or edit the markers that indicate
where solutions are expected to begin and end.  When complete,
submit (just) this file to the HW4 Submissions dropbox on D2L.
Special rules apply for this set of exercises in that, unless
otherwise indicated, we will only be looking for valid answers
and do not require that you provide explanations or testing.
Nevertheless, you should still make sure that you do understand
all of the definitions you write; any of these could be the
subject of a question in an exam later in the term.  Some of
the exercises specify additional requirements in the form of
comments; be sure to follow those requirements before you delete
the lines of code that they are attached to.

To work with this file, you will need either Hugs or the
Glasgow Haskell interpreter (ghci).  You should already be
familiar with using Hugs in previous labs.  If you wish to
use Haskell on your own machine, you will probably find it
easier to install ghci; please visit www.haskell.org/ghc
for more details.

We encourage you to run the interpreter and an editor for
this file in separate windows so you can see both at the
same time.  Start the interpreter by entering either hugs
or ghci at the prompt, and then you can load this file by
typing ":l TypesInHaskell.lhs".  You can then explore the
definitions in the script by typing expressions at the prompt.
If you want to change a definition, you need to edit and
save the script file, and then use the ":r" command to reload
it in the interpreter.  To exit from the interpreter, use the
":q" command:

WARNING: All functions and constants defined at top-level of
a Haskell source file are (potentially) _mutually recursive_;
i.e., they can refer to each other no matter what order they
appear in the file.  Also, no name can be defined twice
in the same file.  This means you have to be careful when
making additions to this file: don't accidentally re-use a
name already defined above or below where you're editing!

-----------------------------------------------------------------------------
INTRODUCTION

Haskell is a _functional_ language, where the word "functional"
has at least two meanings:

- The language is designed to make it convenient to define and
  manipulate functions (we sometimes say functions are "first
  class values").

- The language is (almost) free of side-effects, so computations
  can be structured as applications of functions to arguments,
  just as in mathematics.

Haskell is one of several full-featured functional languages.
Other languages with a strong functional flavor include Lisp,
Scheme, ML, Scala, F#, Clojure, etc.  Haskell is named after
Haskell B. Curry, a pioneering American logician who lived
1900-1982.

Haskell is a general purpose programming language.  Due to the
nature of this course, we have been focussing on applications
related to programming languages such as reduction, evaluation,
grammars, parsing, etc.  However, Haskell is also used for
many other tasks from databases and web servers to gaming,
graphics, utilities, and scripting.

Our interest in this lab is Haskell's _type system_, which has
a particularly clean design. This is largely independent of the
language's functional character.  Haskell is statically typed,
which distinguishes it from Lisp, Scheme, and other dynamically
typed languages like Python, Javascript, Ruby, PHP, etc.

Much of Haskell's type system is shared by the ML language
and its descendents such as OCaml, although Haskell offers
some features that ML lacks.

A distinctive characteristic of Haskell is that it uses
_lazy evaluation_ (expressions are not computed unless 
and until their values are needed). But we won't be relying 
on this feature in this lab.

Indeed, there are many aspects of Haskell that are not covered
in this file: our attention is very much just on types.

-----------------------------------------------------------------------------
PRIMITIVE ATOMIC TYPES

Some primitive atomic types:

Char      -- single characters such as '0' and 'a'
Int       -- fit in a machine-word
Integer   -- unbounded integer 
Float     -- single precision floating point numbers
Double    -- double precision floating point numbers

Some important non-primitive types that are defined in the
standard library (called the Prelude):

Bool      -- includes the Boolean values True and False
String    -- special syntactic support for literals
[t]       -- lists with elements of type t; more special syntactic support

We will see later how these types are defined.

In Haskell, all values are _immutable_: once a value has been
constructed, its contents never change.

-----------------------------------------------------------------------------
CONSTANTS

Some simple constants (must start with a lower-case letter):

> x :: Int     -- a type declaration (almost always optional, but a good idea)
> x = 32768    -- a variable binding

> normal :: Float
> normal = 98.6

> y :: Char
> y = 'B'

> w5 :: String
> w5 = "Hello"

> t :: Bool
> t = True 

> ns :: [Int]
> ns = [1,2,3] -- a list

> ms :: [Int]
> ms = []      -- an empty list

After loading this file, try typing some of the following
expressions (or commands, for the examples beginning :t) at
the interpreter prompt.  You may be familiar with many of these
from the previous labs, so focus on the ones that are new to you.

  x                              -- evaluates a variable
  x + 1                          -- or an arbitrary expression
  21 * 2
  :t x                           -- :t displays the type of a variable
  :t x + 1                       -- or an arbitrary expression
  (normal * 3 + 1) / (2 - normal)-- we have usual arithmetic operators
  (x + 1) `div` 2                -- `div` is integer division operator
  10 `div` 0                     -- generates checked runtime error
  not t                          -- boolean operators: &&,||,not
  normal < 100.0                 -- we have the usual relational operators
  if y == 'B' then 10 else 42    -- if-then-else is an expression form
  let y = 100 in y + 1           -- let is a binding expression form
  w5 ++ w5                       -- concatenation
  head ns                        -- returns first element of a list
  tail ns                        -- returns all but first element of a list
  head ms                        -- generates checked runtime error
  ['b',True]                     -- static error: all list elements must 
                                 --     have the same type

-----------------------------------------------------------------------------
FUNCTIONS

Functions are also a primitive type in Haskell.

A function that expects an argument of type t1 and returns a
result of type t2 is described by the _arrow_ type  t1 -> t2

Simple function definitions (names must also begin with a
lower-case letter).  Notice the lack of parentheses around
parameters.

> faren :: Float -> Float
> faren c = 32.0 + c * (9/5)

> cap :: Char -> Bool
> cap c = c >= 'A' && c <= 'Z'  -- usual ordering

> bookend :: String -> String -> String    -- function w/ two parameters
> bookend a b = a ++ b ++ a          

> inc :: Int -> Int
> inc = \x -> x + 1  -- RHS is lambda expression defining anonymous function

Try evaluating these expressions at top level, ideally trying to
anticipate what the results will be before you hit enter.  The
comments on the right may provide some useful hints/reminders:

  faren (100)            -- parentheses are ok
  faren 100              -- but not needed (so best omitted!)
  faren (50 + 50)        -- unless for grouping
  faren 50 + 50          -- function application binds tighter than + 
  faren                  -- static error: we cannot print function values
  cap 'y'            
  cap y                  -- top-level values are available
  cap y y                -- static error: too many arguments
  bookend "hello" "goodbye"  -- multiple arguments are space-separated
  bookend "hello" 'a'    -- static error: wrong argument types
  bookend "hello"        -- static error: not enough arguments =>
                         --   a mysterious error message (don't worry for now)
  :t bookend             -- functions have types just like everything else
  (+) 2 3                -- operators are just functions in infix position
  div 3 2                -- `foo` is just the infix version of foo

-----------
EXERCISE 1:
  
Write and test a function

thirdInt :: [Int] -> Int

that returns the third element of its (integer list) parameter.
Hint: use two functions we have already seen.

The best way to do this is to edit this file, adding the function
right below this comment.  Then use the :r command to reload
this file into the interpreter.  If your definition generates
static errors, edit it and try :r again.

Once your definition has been accepted by the interpreter, 
you can test it by typing an expression at top level, as usual.
The tests are for your benefit. It is _not_necessary to include
tests in this file. You will _not_ be graded on your tests, only 
on your function definitions.

-- SOLUTION:

> thirdInt :: [Int] -> Int
> thirdInt l = head(tail(tail l))

-- END OF SOLUTION

Let's look a little more carefully at multi-parameter functions,
such as bookend.  We can actually choose to apply this function
to just one argument:

> bookend1 = bookend "hello"

Try evaluating these:

  :t bookend1                     -- String -> String 
  bookend1 "goodbye"              -- acts just like: bookend "hello" "goodbye"
  bookend1 "hello" "goodbye"      -- static error: too many arguments

The result of applying a two-parameter function (say f) to
a single argument is a one-parameter _function_ (call it g).
We can then apply g to a second argument and get back the same
thing as if had we applied f to both arguments.

In fact, at its core Haskell really only has single-parameter
functions.  When we write a type like

  String -> String -> String 

it really means

  String -> (String -> String)

That is, the -> type constructor groups to the right (it is
"right-associative.")

On the other hand, when we write

   bookend "hello" "goodbye"

it really means

   (bookend "hello") "goodbye"

which is exactly the same thing as

   bookend1 "goodbye"

That is, function application groups to the left (it is
"left-associative.")

These two grouping conventions go together to give us
the illusion of multi-parameter functions.  This trick
is called "currying," in further honor of Haskell Curry,
but he didn't really invent it; it should probably be called
"Schoenfinkelisation" instead.  It relies heavily on Haskell's
ability to return functions as the results of other functions;
we say that functions are "first-class" values in Haskell.
(More on this in future labs.)

Aside: now we can see why trying to evaluate

bookend "hello"

gave us an error earlier; there's nothing wrong with this
expression, but since it has a function value we cannot print it.

Currying and partial application generalize to arbitrary numbers
of parameters.  Note that partial application only works on a
prefix of the parameters; you cannot partially apply to, say,
just the second parameter out of three.

-----------
EXERCISE 2:

To do this exercise, replace the first character of each code line
with a '>' character and then fill in the missing parts of code
(marked "...").

Write a function 

  between :: Int -> Int -> Int -> Bool
  between low high val = if val >= low && val <= high then True else False

that returns True iff the third argument value lies between
the first two argument values (inclusive).

Then, write a functions

  validHour   :: Int -> Bool
  validHour    = between 0 24  -- do not change anuthing to the left of the = sign!

  validMinute :: Int -> Bool
  validMinute = between 0 60   -- do not change anything to the left of the = sign!

that return True iff the argument is a valid value for the 
hour and minutes fields, respectively, of a digital clock.
[Hint: use partial application!]

-- SOLUTION:

> between :: Int -> Int -> Int -> Bool
> between low high val = if val >= low && val <= high then True else False

> validHour   :: Int -> Bool
> validHour    = between 0 23  -- do not change anuthing to the left of the = sign!

> validMinute :: Int -> Bool
> validMinute = between 0 59   -- do not change anything to the left of the = sign!

-- END OF SOLUTION

-----------------------------------------------------------------------------
TYPE CONSTRUCTORS: TUPLES

We can build constructed types out of primitive types.
The simplest constructor builds tuples.

A tuple type is just a cartesian product of arbitrary types.
The same syntax is used for both type construction and 
value construction.

> p :: (Int,Bool)
> p = (42,True)

> q :: (String,Int,[Float])
> q = ("Hello",99,[2.81828,3.141592])

> r :: (Float -> Float, Float)
> r = (faren,-273.0)

The elements of tuples are extracted by _pattern matching_.
A pair pattern looks like
  (x,y) 
where x and y are new variable names that are to be bound
to the first and second elements of the pair, respectively.

> s :: (Bool,Int) 
> s = let (p1,p2) = p in         -- pattern match in let binding
>     (not p2, p1 + 5)

> swapIntBool :: (Int,Bool) -> (Bool,Int)
> swapIntBool (x,y) = (y,x)      -- pattern match in function definition

Try evaluating these expressions at top level:

  s
  swapIntBool p
  swapIntBool q                -- static error: wrong tuple size
  swapIntBool r                -- static error: wrong tuple types
  let (f,x) = r in f x 
  (1+2,3+4) == (3,7)      -- equality on tuples is structural
  (77,78,79) < (77,78,78) -- order is lexicographic

-----------
EXERCISE 3:

Using either pattern matching or previously described functions,
write and test a function

  second'n'third :: [Int] -> (Int,Int)

that returns a pair containing the second and third elements
of its (list) argument.

-- SOLUTION:

> second'n'third :: [Int] -> (Int,Int)
> second'n'third l = (head(tail(l)), thirdInt l)

-- END OF SOLUTION

-----------------------------------------------------------------------------
POLYMORPHISM

Often, functions that manipulate constructed types (such as
tuples) don't really care what base types the constructor
is applied to. For example, the following function to swap
(Float,Int) pairs:

> swapFloatInt      :: (Float,Int) -> (Int,Float)
> swapFloatInt (x,y) = (y,x)

This looks exactly like the swapIntBool function, except for the
type declaration. Writing separate versions of such functions
for every instance of pairs we might want would be verbose and
painful! (This is a complaint that lovers of dynamic typing
like Python frequently make about statically typed languages.)

In Haskell, we can instead write a function like swap just _once_
and give it a _polymorphic_ type that allows it be applied to
all kinds of pairs.  ("Polymorphic" means "having many forms.")

A polymorphic type is written using _type variables_, which are
(lower-case) identifiers that can be _instantiated_ to any type,
as long as this is done consistently.

For example, we can define:

> swap      :: (t,u) -> (u,t)
> swap (x,y) = (y,x)

and then simply write

  swap p                     -- t = Int, u = Bool
  swap ("Hello","Goodbye")   -- t = String, u = String
  swap (swap p)              -- outer: t = Bool, u = Int

   etc. 

Much of the Haskell Prelude consists of polymorphic functions
like this.  For example, most functions on lists (like head,
tail, etc.) are polymorphic in the type of list elements. For
example:

  head :: [a] -> a
  tail :: [a] -> [a]

This form of polymorphism, where the definition of the function
is uniform for all types, is sometimes called "parametric
polymorphism." It is closely related to "generics" in Java and C#
and (basic) "templates" in C++.

-----------
EXERCISE 4:

(a) Write and test a polymorphic version of the 'thirdInt'
    function.  Call your function `third`.

(b) Write and test a function 

    splice :: [a] -> [a] -> [a]

    that returns a list consisting of the head of the first
    argument and the tail of the second.

(c) Write and test a function with the following type:

    foo :: a -> b -> a

    How much choice do you have about what 'foo' does?

-- SOLUTION:

> third :: [int] -> int
> third l = head(tail(tail l))

> splice :: [a] -> [a] -> [a]
> splice l1 l2 = head l1 : tail l2

> foo :: a -> b -> a
> foo a b = a

-- END OF SOLUTION

-----------------------------------------------------------------------------
OVERLOADING AND TYPE CLASSES

There is another useful form of polymorphism, sometimes
called "ad hoc" polymorphism or "overloading," in which the
behavior of the function _differs_ depending on the type being
operated on. For example, it is very useful to be able to use an
equality operator (==) on many different types, but the actual
implementation of that operator needs to be type-dependent
and in some cases (e.g. for function types), it shouldn't be
defined at all.

Haskell supports this kind of overloading using a mechanism
called _type classes_.  Each class specifies a set of operations
that should be supported by types that are members of the class;
when we declare a type to be an _instance_ of a class,  we must
give an implementation of the class operations.  Class membership
becomes a _constraint_ on the type of functions that use those
operations.  For example, the == operator is part of the Eq
class, so when we use == in a function, we must make sure that
the type of its operands is an instance of Eq.

For example:

> eqpair :: Eq a => (a,a) -> Bool
> eqpair (a,b) = a == b

Try typing the following:

  eqpair (normal,normal)  -- ok: the type of normal (Float) is an Eq instance
  eqpair (True,True)      -- ok: the type of True (Bool) is an Eq instance
  eqpair (second,second)  -- static error: the type of second ([a] -> a) 
                                           is not an Eq instance
                                    
Another useful built-in typeclass is Show, which specifies
an operation for displaying values as text. (This is used
implicitly by the interpreter to display the results of
top-level expressions.  Note that there is no instance of Show
for function types!)

It is also possible define new instances and new classes,
but we won't see those details in this lab.

-----------------------------------------------------------------------------
TYPE DECLARATIONS

New names for old types.

It is often convenient to give a new name to a constructed type
by using a _type declaration_.

Note that these names are just abbreviations for existing
types, not fundamentally new and different types.  Basically,
the abbreviations just get expanded out before type-checking.
(Hence, these declarations cannot be recursive.)

Type declarations can also be parameterized by type variables.

Note that type names must begin with an upper-case letter.

> type MyPair = (Int,Bool)
> type YourPair = (Bool,Int)
> type OurSwap = MyPair -> YourPair
    
> p' :: MyPair
> p' = (10, True)

> swapIntBool' :: OurSwap
> swapIntBool' (x,y) = (y,x)

> type PairIntWith a = (Int,a)
> type Swap a b = (a,b) -> (b,a)

> p'' :: PairIntWith Bool
> p'' = (10, True)

> p1 :: PairIntWith Float
> p1 = (10, 3.14) 

> swapIntBool'' :: Swap Int Bool
> swapIntBool'' (x,y) = (y,x)

Try evaluating these expressions:

  swapIntBool' p'
  swapIntBool p'
  swapIntBool' p
  swapIntBool' p''
  swapIntBool'' p
  p == p'
  p == p''
  p'' == p1     -- static type error

The built-in String type is actually defined (in the Prelude) as 

  type String = [Char] 

-----------
EXERCISE 5:

Write and test a function:

   thirdString :: String -> Char 
   thirdString = ...   -- do not change anything to the left of the = sign!

that returns the third character of a String.

-- SOLUTION:

> thirdString :: String -> Char 
> thirdString = third

-- END OF SOLUTION

-----------------------------------------------------------------------------
ALGEBRAIC DATA TYPES

The key mechanism for building genuinely new types in Haskell
is the _algebraic data type_ definition.

Algebraic data types combine:

- cartesian products 
- sums (disjoint unions)
- recursion

into a single unified mechanism for declaring types, constructing
values, and deconstructing them again (using pattern matching).

As a characteristic example, consider the type of binary trees,
with integers at the internal nodes.  In Haskell, we can write:

> data ITree = ILeaf
>            | INode Int ITree ITree   
>      deriving (Eq, Show)

This definition does two things:

- It defines ITree to be a completely new type.

- It defines two _constructors_ for this type, ILeaf and INode,
  which can be used to create values _and_ to pattern match on
  values in order to deconstruct them.

(The "deriving" clause makes ITree an instance of the
 Eq and Show type classes.)

Taken as a whole, ITree is a _sum type_: every value of the type
must be _either_ an ILeaf _or_ an INode, and we can distinguish
which kind of value it is by the constructor that created it.

The list of types after each constructor name specifies the
number and types of data fields stored in a value created with
this constructor.  So ILeaf has no fields and and INode has
three fields (one Int and two ITree's).  INode represents a
_product type_: each INode value contains a value for each of
the three constituent field types.

ITree is also a _recursive_ type, since its constructor INode
has fields of the same type we're defining!

When used to create values, ILeaf and INode behave just like
functions.  You can ask the top level for their types:

:t ILeaf
:t INode

These are much like constructors in Java or other OO languages,
except that they do not execute arbitrary code: they _just_
construct a value from its constituent fields. For example,
we can create the small tree

                 2
                / \
               /   \
              1     7
             / \   / \
            /   \ -   -
           4     5
          / \   / \
         -   - -   -              
thus:

> t1 :: ITree
> t1 = INode 2 (INode 1 (INode 4 ILeaf ILeaf)
>                       (INode 5 ILeaf ILeaf))
>              (INode 7 ILeaf ILeaf)                              

To inspect an ITree value, we use the same constructor names
in a multi-way _pattern match_, which can be specified using
a _case_ expression.

> sumITree :: ITree -> Int
> sumITree t
>   = case t of                   
>       ILeaf -> 0
>       INode n left right -> n + sumITree left + sumITree right

The case expression works by comparing its argument against
each pattern in turn, looking for the first one that matches,
and then evaluating the corresponding expression after the ->

Each case arm has the form

 C x1 ... xn -> e

where:

  C is a constructor name
  x1,...,xn are fresh variable names to be bound within e to the 
            fields of the constructed value 
  e is an expression giving the value to return if this arm matches.

WARNING: Haskell syntax is indentation-sensitive, so it is
important that the two arms of the case start in the same column.

-----------
EXERCISE 6:

Write a function :

  doubleITree :: ITree -> ITree

that produces a copy of its argument in which each node value
has been doubled (i.e., multiplied by two).

-- SOLUTION:

> doubleITree :: ITree -> ITree
> doubleITree t = case t of 
>                   ILeaf -> ILeaf
>                   INode n left right -> INode (2*n) (doubleITree left) (doubleITree right)

-- END OF SOLUTION

-----------------------------------------------------------------------------
 PARAMETERIZED TYPES

Like type abbreviations, algebraic data type definitions can
also be parameterized by type variables.

For example, we can readily generalize our binary trees to
hold arbitrary kinds of values, and indeed to hold _different_
kinds of values at the internal nodes vs.  at the leaves.

> data Tree a b = Leaf a 
>               | Node b (Tree a b) (Tree a b)
>      deriving (Eq, Show)

> t2 :: Tree Bool Int
> t2 = Node 17 (Node 2 (Leaf True)
>                      (Node 7 (Leaf False)
>                              (Leaf False)))
>              (Leaf True)                              

-----------
EXERCISE 7:

Write and test a function

  depthTree :: Tree a b -> Int
   
that calculates the depth of the tree in its argument.
A single Leaf node has depth 1, while the depth of a node
is 1 plus the maximum depth of its children.  (For example,
depthTree t2 should return 4.)  Note that this function doesn't
depend on the types of data stored in the tree's nodes.

[Hint: You can use max x y to calculate the maximum of two
values x and y.]

-- SOLUTION:

> depthTree :: Tree a b -> Int
> depthTree t = case t of 
>                   Leaf a -> 1
>                   Node _ left right -> 1 + max (depthTree left) (depthTree right)

-- END OF SOLUTION
         
We could also redefine our previous ITree as an instance of
this more general tree:

> type ITree' = Tree () Int

Here () denotes the built-in _unit_ type, which contains just
a single value, also written ().  Having a value of this type
gives you essentially no information, since it _must_ be the
one value () !

> t1' :: ITree'
> t1' = Node 2 (Node 1 (Node 4 (Leaf ()) (Leaf ()))
>                      (Node 5 (Leaf ()) (Leaf ())))
>              (Node 7 (Leaf ()) (Leaf ()))                              

> sumITree' :: ITree' -> Int
> sumITree' t
>   = case t of                   
>       Leaf _ -> 0
>       Node n left right -> n + sumITree' left + sumITree' right

-----------------------------------------------------------------------------
LISTS

The built-in list type is actually defined (in the Prelude)
essentially as follows, except for syntactic differences:

> data List a = Nil                  -- like []
>             | Cons a (List a)      -- like :
>      deriving (Eq, Show)

> ns1 :: List Int
> ns1  = Cons 1 (Cons 2 (Cons 3 Nil))  -- like [1,2,3]

This says that a list is either empty (Nil) or is built by by
prepending a new element to an existing list (Cons).

Here "Nil" and "Cons" are conventional names inherited from
the LISP language.  Actual Haskell lists use

[t]   for List t
[]    for Nil
x:xs  for Cons x xs

and provide additional "syntactic sugar" for writing literal
lists, e.g.

[1,2,3] for Cons 1 (Cons 2 (Cons 3 Nil))

Functions over lists are defined by pattern matching and
recursion. Notice that patterns can be nested, and _ can be used
as a "wild-card" in patterns when we don't care about a value.

> myhead :: [a] -> a
> myhead xs
>   = case xs of
>       [] -> error "head on empty list"
>       x:_ -> x

> mytail :: [a] -> [a]
> mytail xs
>   = case xs of
>       [] -> error "tail on empty list"
>       _:xs' -> xs'

> mythird :: [a] -> a
> mythird xs
>   = case xs of
>       _:_:x:_ -> x
>       _ -> error "third on short list" 

> mylength :: [a] -> Int
> mylength xs
>   = case xs of
>       [] -> 0
>       _:xs' -> 1 + mylength xs'

-----------
EXERCISE 8:

Write and test a function:

  myzip :: [a] -> [b] -> [(a,b)]

that "zips" two lists into a corresponding list of pairs.
For example:

  myzip [1,2,3] [4,5,6] = [(1,4),(2,5),(3,6)]

Do something sensible if the two argument lists are of different
lengths.

Hint: You can case over a _pair_ of values at once.

-- SOLUTION:
TODO

> myzip :: [a] -> [b] -> [(a,b)]

> myzip _ [] = []
> myzip [] _ = []
> myzip (a:as) (b:bs) = (a, b) : myzip as bs                                        

-- END OF SOLUTION

First attempt using case statement

 myzip l1 l2 = case (l1 l2) of 
                   [] _ -> []
                   (a:as) (b:bs) -> (a, b) : myzip as bs



-----------------------------------------------------------------------------
ABSTRACT SYNTAX TREES

As a more elaborate example, here is the algebraic data type
encoding of the AST of a little language of integer expressions.
(It should remind you of the definition for the Prop type that
we saw in Week 1!)

> data Exp = NumE Int
>          | VarE String
>          | LetE String Exp Exp
>          | AddE Exp Exp
>          | SubE Exp Exp
>          | IfnzE Exp Exp Exp
>    deriving (Eq, Show)

> e1 :: Exp 
> e1 = LetE "y" 
>           (SubE (NumE 5) 
>                 (AddE (NumE 2)
>                       (NumE 3)))
>           (IfnzE (VarE "y")
>                  (NumE 10)
>                  (LetE "y" 
>                        (NumE 21)
>                        (AddE (VarE "y")
>                              (VarE "y"))))
> 

Notice that our little language has 'let' expressions, inspired
by the ones in Haskell itself.  In Haskell, an expression of
the form

   let x = e in e1 

has the effect of setting x to the value of e while evaluating
e1.  For example, 'let x = y+1 in x*x' is equivalent to
'(y+1)*(y+1)' Note that the scope of variable x is just e1.
The 'LetE' expressions in our little language are intended to
behave the same way.

To write an evaluator for the little language, we will need to
implement a mapping from variable names to their values. This
is often called an _environment_.  One simple way to represent
an environment is by a list of (key,value) pairs.

> type Env a = [(String,a)]

We can use the extendEnv function to extend an environment with
a new variable name and value pair:

> extendEnv :: Env a -> String -> a -> Env a
> extendEnv env k v = (k,v):env

And we can use the lookupEnv function to find the value that is
associated with a given variable name in a specified environment:

> -- if the given key k is not found, return the default d
> lookupEnv :: Env a -> String -> a -> a
> lookupEnv env k d
>   = case env of
>       (k',v'):env' -> if k == k' then v' else lookupEnv env' k d
>       [] -> d

The empty environment does not contain bindings for any variables:

> emptyEnv :: Env a
> emptyEnv  = []

With environments in place, the actual evaluation function
is easily written, with the aid of pattern matching.

> eval :: Env Int -> Exp -> Int
> eval env e
>   = case e of
>       NumE n -> n
>       VarE x -> lookupEnv env x 0
>       LetE x e1 e2 -> 
>         let v1 = eval env e1 in
>         eval (extendEnv env x v1) e2
>       AddE e1 e2 -> eval env e1 + eval env e2
>       SubE e1 e2 -> eval env e1 - eval env e2
>       IfnzE e1 e2 e3 -> 
>         let v1 = eval env e1 in
>         if v1 /= 0 then eval env e2 else eval env e3

> evalProgram :: Exp -> Int
> evalProgram exp = eval emptyEnv exp

Play around with this code, e.g. try evaluating the test tree
and some variants of it.

Here is the AST datatype for a similar little language,
this time for boolean expressions containing: 
- the constants True and False
- let bindings and (boolean-valued) variables, behaving
  as in the first little language 
- the operators And, Or, and Not

> data BExp = BoolB Bool   -- represents the constants
>           | VarB String
>           | LetB String BExp BExp 
>           | AndB BExp BExp
>           | OrB BExp BExp
>           | NotB BExp
>    deriving (Eq, Show)

(Again, this definition should remind you of Prop, but you
should also be able to see that there are one or two interesting
differences between Prop and BExp.)

-----------
EXERCISE 9:

Write down an evaluation function for this boolean little
language, of type

  beval :: Env Bool -> BExp -> Bool

An unbound variable should evaluate to False.  Be sure to test
your code (although your tests won't be graded).

-- SOLUTION:

> beval :: Env Bool -> BExp -> Bool
> beval env b = 
>       case b of 
>           BoolB b -> b
>           VarB x -> lookupEnv env x False    
>           LetB x b1 b2 ->
>               let v1 = beval env b1 in
>               beval (extendEnv env x v1) b2
>           AndB b1 b2 -> beval env b1 && beval env b2
>           OrB b1 b2 -> beval env b1 || beval env b2
>           NotB b1 -> 
>               let v1 = beval env b1 in
>               if v1 then beval env (BoolB False) else beval env (BoolB True)

-- END OF SOLUTION

> b1 :: BExp
> b1 = LetB "b"
>           (AndB (BoolB True)
>                 (OrB (BoolB False)
>                      (NotB (BoolB True))))
>           (VarB "a")


-----------------------------------------------------------------------------
SPECIAL CASES

The algebraic data type mechanism is very powerful and general,
but some its most important uses occur in restricted special
cases.

OPTIONAL VALUES

The Prelude defines the following data type, which can be
thought of turning any type t into an "optional" t --- either
there really is a value of type t present, or there isn't.

  data Maybe a = Nothing
               | Just a

Among other things, this sum type is very useful for signaling
error conditions in the return value of a function, e.g. for this
function, which looks for a match in a list of (key,value) pairs:

> search :: [(Int,a)] -> Int -> Maybe a
> search xys x0
>   = case xys of
>       [] -> Nothing
>       (x,y):xys' -> if x0 == x then Just y else search xys' x0

> example :: Int -> String
> example x
>   = case search [(1,"a"),(2,"b")] x of
>       Just y -> "Found: " ++ y
>       Nothing -> "Not Found"

Try these out!

The Nothing value often plays a role analogous to the null object
value in Java. The difference is that here we can _choose_ on
a case-by-case basis whether to wrap Maybe around an argument
or result type. And if we do use Maybe, we are forced to do a
pattern match to see if we got a value or not.

ENUMERATIONS

A data type whose constructors carry no values is just like an
enum type in other languages.  For example:

> data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
>    deriving (Eq,Show)

> weekend :: Day -> Bool
> weekend d
>   = case d of
>       Sat -> True
>       Sun -> True
>       _ -> False

The built-in Boolean type is just a particularly simple form
of enumeration:

  data Bool = False | True

The if-then-else construct is just special syntax for a case
over Bool expressions.

Simplifying in another direction, an algebraic data type with
just one constructor is essentially just a record.

> data Emp = MkEmp String Int  -- employee name, id#
>    deriving (Eq,Show)

> emp :: Emp
> emp  = MkEmp "Sam" 99

RECORDS
   
Haskell provides additional syntax to allow record fields to
be named; when used, this feature also automatically defines
_selector functions_ for extracting fields by name.  For example:

> data Empr = MkEmpr { name :: String, idnum :: Int }
>    deriving (Eq, Show)

This generates exactly the same type and constructor as Emp,
but allows construction using named fields.

> empr :: Empr
> empr  = MkEmpr {name = "Sam", idnum = 99 }

and defines selector functions name and idnum. 

Try the following in the interpreter:       

  :t name
  :t idnum   
  name empr

An algebraic data type with just one constructor can be useful
for imposing distinctions on structurally equivalent types.
For example, suppose we have a program that manipulates complex
numbers in both polar and rectangular representations (Google
"polar coordinate system" if you don't know about these.)
Each representation is a pair of floating point numbers, but
the _meaning_ of the numbers is different in the two cases ---
so it can be useful to distinguish them as two separate types.

> type Fpair = (Float,Float)

> data Polar = P Fpair
>      deriving (Eq, Show)
> data Rect = R Fpair
>      deriving (Eq, Show)

> polarMul :: Polar -> Polar -> Polar
> polarMul (P(r1,th1)) (P(r2,th2)) = P (r1*r1,th1+th2)

> rectAdd :: Rect -> Rect -> Rect
> rectAdd (R(x1,y1)) (R(x2,y2)) = R (x1+x2,y1+y2)
 
Try out these expressions:

   polarMul (P (150.0,30.0)) (P(10.0,20.0))
   rectAdd  (R (150.0,30.0)) (R(10.0,20.0))
   polarMul (P (150.0,30.0)) (R(10.0,20.0))   -- type error
   [P (10.0,20.0), R (20.0,30.0)]             -- type error

(Notice that this last error is a little sad: we might want to
be able to mix both kinds of coordinates in a list, but this
is disallowed because R and P are constructors of _different_
types.)

------------
EXERCISE 10:

Define single-constructor data types for Farenheit and Celsius
temperatures as Float values, and use them to write and test
a conversion function 'ftoc' from F to C.

[Reminder: 32 degrees Fahrenheit corresponds to zero Celsius, and
an increase of 9 degrees Fahrenheit corresponds to an increase
of 5 degrees Celsius.  A conversion function in the opposite
direction was included in an earlier section of this document.]

-- SOLUTION:

> data Fahr = F Float
>       deriving (Eq, Show)
> data Cels = C Float
>       deriving (Eq, Show)

> ftoc :: Fahr -> Cels
> ftoc (F(f)) = C ((f-32)*(5/9))

-- END OF SOLUTION

Finally, the unit type mentioned before is just an algebraic
data type with one constructor carrying no values.  Again,
this type has just one value, its (nullary) constructor.

> data MyUnit = U

We can even write an algebraic data type with _no_ constructors
at all.  This describes a type with no values!

> data Void

-----------------------------------------------------------------------------
TYPE INFERENCE

Haskell has a powerful type inference engine, which can figure
out the types of most expressions and functions automatically,
without the aid of programmer declarations.

This eases the burden for programmers (and addresses another
of the complaints that dynamic-typing enthusiasts have about
statically-typed languages).  It is particularly useful for
local declarations, such as the 'let' declarations we have used
a few places in this file, since here the type is often obvious
from context and therefore especially painful to write down.

On the other hand, we have carefully given an explicit type
declaration for every top-level definition in this file.
For functions, at least, this is often considered good standard
practice, because it provides a useful form of documentation,
and can help programmers be sure that they are defining the
function they think they are!

Note that Haskell always infers the _most general_ (i.e.
most polymorphic) type that is valid for an expression.

Try this out on a bare definition of our favorite function on
pairs; see what :t says about its type.

> yetAnotherSwap (x,y) = (y,x)

-----------------------------------------------------------------------------
Additional Exercises:

------------
EXERCISE 11:

Write down Haskell 'data' definitions for the following
informally described types.  ALSO, for each definition include
a comment that says which of the following kinds of type
constructions is in use: products, sums, recursion, function.

Note: Be sure to choose constructor names that do not conflict
with existing ones in this file!

(a) Type A represents 2-3 trees containing Int values.  A 2-3
    tree is a kind of search tree (suitable for representing sets
    or dictionaries), in which every internal node has either one
    data element and two children, or two data elements and three
    children; leaf nodes have no data.

(b) Type B represents boolean bit vectors, where a bit vector is
    (i) empty, or (ii) a single boolean, or (iii) the concatenation of
    two bit vectors.

(c) Type C is an enumeration type for the twelve months of the year.

(d) Type D represents details of a flight record in an airline
    database that captures a flight number (Int), the starting
    and ending cities (String), and an average ticket price
    (Float).

(e) Type E represents temperatures, including representations for
    both Fahrenheit or Centigrate temperature values expressed as
    Double precision floating point numbers.

(f) Type F represents an arbitrary pair of functions with the
    only constraint being that the domain and range types of
    the first function in each pair must match the range and
    domain types, respectively of the second function.  (For
    example, in some cases, the second component might be an
    inverse of the first component.)

-- SOLUTION:

> data BitVec b =  Empty | SingleBool Bool | TwoVecs Bool Bool
>   deriving (Eq, Show)

> data MoY = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
>   deriving (Eq, Show)

> data FlightR = FR { flight_number :: Int, start_city ::String, end_city :: String, avg_price :: Float }
>   deriving (Eq, Show)

> data Temps = T { fahrenheit :: Double, centigrate :: Double }
>   deriving (Eq, Show)

-- END OF SOLUTIONS

------------
EXERCISE 12:

Write short answers to the following questions. Fill in your
answers in the spot shown below each question, staying within
this comment.

(a) In Java, the set of values for each object type (defined by a
class or interface declaration) always includes the special value
'null'.  So are Java object types essentially like products or
like sums?  Explain briefly.

-- SOLUTION:

Java objects are more like products because an object naturally combines all of the values into
an aggregate value. The language injects the null value for us, though if it didn't we could just
as easily do so ourselves.

-- END OF SOLUTION

(b) Int is a built-in type in Haskell, representing integers
that fit within a fixed bit width. (The exact width w is
machine-dependent; The Haskell standard specifies that it will be
at least 29 bits.)  If Ints were not built in, we could define
them as a constructed type. Would it be better to represent 
each Int as a product or as a sum?  Explain briefly.  In terms
of types, why do you think the Haskell standard does not specify
that an Int will have at least 32 bits on a machine with 32
bit words?

-- SOLUTION:

It would be better to represent them as sums 
-- END OF SOLUTION

(c) What common mathematical set is represented by the following
data declaration? Explain briefly.

  data N = Z 
         | S N

-- SOLUTION:

This mathematical set is the natural numbers. After doing a little research I realized that
it's a recursive declaration where a natural number N is represented by building up to it.
I.e. starting at zero and taking the next number until we've built up to a number N.

-- END OF SOLUTION

-----------------------------------------------------------------------------
