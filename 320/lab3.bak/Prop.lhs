----------------------------------------------------------------------

> module Prop where

This is a "literate Haskell" source file, intended to accompany the
Week 1 lecture for CS 320, Principles of Programing Languages.

The term "literate Haskell" means that this is a text file in which
the lines of Haskell code are marked explicitly by a > character
in the first column.  All other lines are treated as comments.
Note also that the literate Haskell file format requires (at least)
one blank line between sections of code and sections of plain text.

Although you are welcome to read and study the contents of this file if
you wish, you are not expected to do that, or to understand all of the
included Haskell code, especially at this early stage of the course.
Instead, if you have downloaded this file to your CS Linux account,
then you should be able to start up a Haskell interpreter and start
typing in expressions to be evaluated by using the following command:

  hugs Prop.lhs

AGAIN, you are NOT expected to read and understand all of the following
code, but you are encouraged to skim over it to get a better sense
of what Haskell programs look like!

----------------------------------------------------------------------
We start by importing some general list processing operators from
one of the standard Haskell libraries:

> import Pic                    -- a library for drawing text pictures
> import Data.List(nub,         -- remove duplicates from a list
>                  sort)        -- sort the elements of a list

-- Abstract syntax for propositions: ---------------------------------

> data Prop = AND Prop Prop
>           | OR  Prop Prop
>           | NOT Prop
>           | TRUE
>           | FALSE
>           | VAR String
>             deriving (Eq, Show)

> listProps :: [Prop] -> IO ()
> listProps  = putStr
>            . unlines
>            . zipWith (\i s -> show i ++ ") " ++ s) [1..]
>            . map show

Here are some functions that define the label and subtrees of each
node in a Prop abstract syntax tree; these functions will be used
when we try to display a Prop value as a tree.

> instance Tree Prop where
>   label (AND p q)    = "AND"
>   label (OR  p q)    = "OR"
>   label (NOT p)      = "NOT"
>   label TRUE         = "TRUE"
>   label FALSE        = "FALSE"
>   label (VAR v)      = v
>
>   subtrees (AND p q) = [p, q]
>   subtrees (OR  p q) = [p, q]
>   subtrees (NOT p)   = [p]
>   subtrees TRUE      = []
>   subtrees FALSE     = []
>   subtrees (VAR v)   = []

-- Computing over abstract syntax trees: -----------------------------
The following function calculates the list of all variables
appearing in a given AST:

> vars          :: Prop -> [String]
> vars (AND p q) = vars p ++ vars q
> vars (OR p q)  = vars p ++ vars q
> vars (NOT p)   = vars p
> vars TRUE      = []
> vars FALSE     = []
> vars (VAR v)   = [v]

-- Environments: -----------------------------------------------------
Environments are mappings from variables to (Boolean) values; we
need to specify an environment when we evaluate an expression, or
else we won't know what values to use for any variables that appear
in the expression.

We will represent environments here as a simple list of pairs,
each of which includes a string (naming a variable) and a Bool
(specifying the value of the variable).  This data structure is
also commonly referred to as an "association list":

> type Env = [(String, Bool)]

----------------------------------------------------------------------
Evaluation of a proposition in a given environment produces
a Boolean result:

> eval              :: Env -> Prop -> Bool
> eval env (AND p q) = eval env p && eval env q
> eval env (OR p q)  = eval env p || eval env q
> eval env (NOT p)   = not (eval env p)
> eval env TRUE      = True
> eval env FALSE     = False
> eval env (VAR v)   = case lookup v env of
>                        Just b  -> b
>                        Nothing -> error ("No definition for " ++ v)

We can also evaluate a list of propositions using a given
evironment:

> evallist       :: Env -> [Prop] -> [Bool]
> evallist env ps = [ eval env p | p <- ps ]

----------------------------------------------------------------------
To generate a truth table for a list of propositions, we need to
find all of the variable names that appear in those propositions;
then generated the list of all possible environments for those
variables (each different environment will correspond to one row in
the truth table); and then calculate the values of each proposition
using each of those environments:

> truthTables :: [Prop] -> Pic
> truthTables ps = table (header : rows)
>  where header = map string uvs ++ [ empty | p <- ps ]
>        rows   = map (map (string . show)) bools
>        bools  = [ map snd env ++ evallist env ps | env <- envs uvs ]
>        uvs    = nub (sort (concat (map vars ps)))

For convenience, we will also provide a version of this function that
constructs a truth table for a single Prop formula:

> truthTable  :: Prop -> Pic
> truthTable p = truthTables [p]

The following recursive definition allows us to construct the list
of all possible environments for a given list of strings:

> envs       :: [String] -> [Env]
> envs []     = [[]]
> envs (v:vs) = [ (v, False):env | env <- envs vs ] ++
>               [ (v, True) :env | env <- envs vs ]

----------------------------------------------------------------------
Given a single environment, there might still be many ways to reduce
an initial Prop, depending on which part of the Prop we want to
evaluate first.

> reduce                      :: Env -> Prop -> [Prop]
> reduce env (AND TRUE  TRUE)  = [TRUE]
> reduce env (AND FALSE TRUE)  = [FALSE]
> reduce env (AND TRUE  FALSE) = [FALSE]
> reduce env (AND FALSE FALSE) = [FALSE]
> reduce env (OR  TRUE  TRUE)  = [TRUE]
> reduce env (OR  FALSE TRUE)  = [TRUE]
> reduce env (OR  TRUE  FALSE) = [TRUE]
> reduce env (OR  FALSE FALSE) = [FALSE]
> reduce env (NOT FALSE)       = [TRUE]
> reduce env (NOT TRUE)        = [FALSE]
> reduce env (AND l r)         = [ AND l' r | l' <- reduce env l ] ++ 
>                                [ AND l r' | r' <- reduce env r ]
> reduce env (OR  l r)         = [ OR  l' r | l' <- reduce env l ] ++ 
>                                [ OR  l r' | r' <- reduce env r ]
> reduce env (NOT p)           = [ NOT p'   | p' <- reduce env p ]
> reduce env (VAR v)           = case lookup v env of
>                                  Just b  -> [ if b then TRUE else FALSE ]
>                                  Nothing -> []
> reduce env _                 = []

----------------------------------------------------------------------
Given the mechanism above for reducing an expression by one step,
we can perform repeated reduction steps on a given input expression
until no more reductions are possible.  This process is called
normalization, and the final expression is called a "normal form".

> normalize      :: Env -> Prop -> [Prop]
> normalize env p = p : case reduce env p of
>                         [] -> []
>                         qs -> normalize env (head qs)

The definition of normalize above always picks the leftmost (i.e.,
first) option in the list produced by reduce at each step, but in
principle we should be able to pick different options at each step
and still end up with a normal form.

> normalizeGen           :: ([Prop] -> Prop) -> Env -> Prop -> [Prop]
> normalizeGen pick env p = p : case reduce env p of
>                              [] -> []
>                              qs -> normalizeGen pick env (pick qs)

For example, we can use normalizeGen head to pick the leftmost reduction
at each step; normalizeGen last to pick the rightmost reduction' and,
with the definition of mid below, normalizeGen mid to choose the middle
option at each step.

> mid   :: [a] -> a
> mid xs = xs !! (length xs `div` 2)

Does every reduction always produce the same final result?  Let's
calculate the set of all normal forms:

> normalForms      :: Env -> Prop -> [Prop]
> normalForms env p = case reduce env p of
>                      [] -> [p]
>                      ps -> concat (map (normalForms env) ps)

-- Enumerating propositions: -----------------------------------------
We can enumerate all of the Prop values that use a given list of
variable names using the following function (which makes use of the
fconcat and cp operators defined below).

> props   :: [String] -> [Prop]
> props vs = ps
>  where ps = fconcat [ [TRUE, FALSE],
>                       [ VAR v | v <- vs ],
>                       [ NOT e | e <- ps ],
>                       [ OR  l r | (l, r) <- cp ps ps ],
>                       [ AND l r | (l, r) <- cp ps ps ] ]

For example, we can enumerate the propositions with no variables
using props [] and we can use props ["A"] to enumerate all of
the valid propositions that use only one variable, called "A".

We can use the fconcat function to enumerate the elements in a
list of lists when we know that the list itself is finite (even
if some of the lists that it contains are not):

> fconcat    :: [[a]] -> [a]
> fconcat xss = case filter (not . null) xss of
>                []  -> []
>                yss -> map head yss ++ fconcat (map tail yss)

If we want to enumerate the values in a list of lists and there is
a possibility that the outer list is infinite, then we will need a
more sophisticated "diagonalization" algorithm:

> diag :: [[a]] -> [a]
> diag  = concat . cols
>  where
>   cols                    :: [[a]] -> [[a]]
>   cols []                  = []
>   cols ([]:xss)            = cols xss
>   cols ((x:xs):xss)        = [x] : formCols xs (cols xss)
>
>   formCols                :: [a] -> [[a]] -> [[a]]
>   formCols (x:xs) (ys:yss) = (x:ys) : formCols xs yss
>   formCols []     yss      = yss
>   formCols xs     []       = [ [x] | x <- xs ]

Diagonalization can be used, for example, to enumerate the cartesian
product of an arbitrary pair of lists, even if one or both of the
lists is not finite.

> cp      :: [a] -> [b] -> [(a, b)]
> cp xs ys = diag [ [ (x,y) | x <- xs ] | y <- ys ]

----------------------------------------------------------------------
