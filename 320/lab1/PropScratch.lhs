-----------------------------------------------------------

> module PropScratch where
> import Pic
> import Prop
> import Data.List

You can add definitions here as you experiment with Prop.

> a = VAR "A"
> b = VAR "B"
> c = VAR "C"
> d = VAR "D"

> ex0 = AND (OR a TRUE) (NOT b)
> ex1 = AND (OR a (NOT b)) (OR (NOT a) b)
> ex2 = OR (OR (NOT a) bczero) silly
> ex3 = OR (AND a (NOT b)) (OR c d)

> bczero = AND (NOT b) (NOT c)
> silly = AND d FALSE 

ex2 = OR (AND (OR (a (NOT b)) (OR (NOT a) b))

An example Prop value using the abstract syntax constructors:

> exp0 = OR (AND (NOT (VAR "A")) (VAR "B"))
>           (AND (VAR "A") (VAR "B"))

Some sample environments:

> env0 = [("A", True), ("B", False)]

> env1
>  = [ ("A", True),
>      ("B", False),
>      ("C", False),
>      ("D", True) ]

Note that this is a literate file.  In particular, this
means that (1) code lines must begin with a ">" in the
first column; and (2) there must be at least one blank
line between any code line and any other line of text.

We can even add definitions that describe new ways to
build circuits:

> xor    :: Prop -> Prop -> Prop
> xor p q = OR (AND p (NOT q)) (AND (NOT p) q)

-----------------------------------------------------------
For reference, the functions that you can use include:

* AND   :: Prop -> Prop -> Prop
  OR    :: Prop -> Prop -> Prop
  NOT   :: Prop -> Prop
  TRUE  :: Prop
  FALSE :: Prop
  VAR   :: String -> Prop

  The "constructors" for the Prop type.

* vars :: Prop -> [String]

  List the variables appearing in a given Prop value.

* listProps :: [Prop] -> IO ()

  Display a list of Prop values, each one on a different
  line with a distinct line number.

* picTree  :: Prop -> Pic

  Draw the abstract syntax tree for a given Prop value.

* picTrees :: [Prop] -> Pic

  Draw the abstract syntax trees for a list of Prop values.

* truthTable :: Prop -> Pic

  Display the truth table for a given Prop value.

* truthTables :: [Prop] -> Pic

  Display the truth tables for a list of Prop values.

* eval :: Env -> Prop -> Bool

  Display the value of a given Prop value using the
  specified environment.  The environment must provide
  a value for every variable in the given Prop.

* reduce :: Env -> Prop -> [Prop]

  Display all the expressions that can be reached from
  the given Prop value in a single step.  Variables
  may be reduced using information in the environment,
  if provided, but are otherwise treated as (unknown)
  constants.

* normalize :: Env -> Prop -> [Prop]

  Attempt to calculate a normal form for a given Prop
  value, returning the list of all the steps in the
  reduction sequence.

* normalForms :: Env -> Prop -> [Prop]

  Return a list of all normal forms that can be obtained
  by changing the order in which expressions are reduced.
  Is it possible for a single expression to reduce to
  different results if you change the order of evaluation?

* props :: [String] -> [Prop]

  Return a list of all Prop values that can be constructed
  using variables whose names appear in the input list.
  This will typically produce a very long list of
  expressions (some might even say "infinite" :-) so you
  are encouraged to use this in combination with the take
  operator, as in:  take 10 (props ["A", "B"])

-----------------------------------------------------------
