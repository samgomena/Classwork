--------------------------------------------------------------------
CS 320 Principles of Programming Languages           Grammar Toolkit

Mark P. Jones and Andrew Tolmach
Portland State University
Last updated: April 8, 2019
--------------------------------------------------------------------

The code in this file provides some functions for calculating and
experimenting with context free grammars.

You will need to read the documentation at the top of this file in
order to make proper use of this file.  You are NOT expected or
required to study the implementation that follows (although, of
course, you are welcome to take a look if you are so inclined).
Remember that the primary purpose of the code in this file is to
help you experiment with definitions of grammars; the fact that it
happens to be written in a functional language is incidental.

GRAMMARS:
---------
Grammars are specified as strings that include a list of productions
with adjacent items separated by semicolons.  Each production has
the form N -> symbols, where N on the left of the arrow is a
nonterminal name and the symbols portion on the right is a list of
zero or more terminal or nonterminal symbols separated by spaces.
Nonterminal names begin with a capital letter followed by zero or
more alphanumeric characters.  Any other sequence of characters (not
including spaces, -> or ;) is treated as a terminal symbol.  The
following strings show two examples:

  "A -> a A ; A ->"
  "E -> num ; E -> E + E; E -> - E ; E -> ( E )"

Although it is possible to enter strings like these directly at the
Haskell prompt, you may find it useful to add definitions to a
scratch file that imports this module so that you can reference
them repeatedly in calculations at the interpreter prompt.

OPERATIONS:
-----------
We describe the main operations provided by the code in this file
in the list below.  We use the parameter names g and n to represent
an arbitrary grammar string, and an integer value, respectively.
Each of the examples that follows uses the grammar g1 as its input;
this is just the grammar "A -> a A ; A ->" that was mentioned above.

----
grammar g  will print a description of the grammar represented by g.
   You can use this to confirm that the grammar string has been read
   in the way intended.

   Example:

    Grammar> grammar g1
    1) A -> a A
    2) A -> 
    Grammar>

----
readGrammar g  will show you the abstract syntax (i.e,, the Haskell
   data structure) the represents the grammar in g.  This will only
   really be useful if you decide to poke under the hood and take a
   look at how this code works.

   Example:

    Grammar> readGrammar g1
    Grammar [("A",[Terminal "a",NonTerm "A"]),("A",[])]
    Grammar>

----
derivationTree n g   will print a tree structure that shows all of
   the possible derivations from the start symbol of grammar g,
   which appears at the root of the tree.  In general, these trees
   are infinite, and grow rapidly, so we have added a parameter n
   to limit the number of levels of the tree that are displayed.
   In practice, you will probably only use very low values of n
   before the trees become unmanageably large (unless you are
   experimenting with a very simple grammar).

   Example:

    Grammar> derivationTree 3 g1
                    A     
                    |     
                .--------.
                |        |
               a A        
                |         
            .--------.    
            |        |    
          a a A      a    
            |             
        .-------.         
        |       |         
     a a a A   a a        
    
    Grammar>

   (Note that the rightmost child of the root node in this example
   is blank, indicating that the empty string is included in the
   language for this grammar.)

----
parseTrees n g  will print a list of the first n parse trees that
   are generated from the grammar g.  This list includes one tree
   for each different derivation, so a single tree may show up
   multiple times if it has multiple derivations (e.g., if it has
   distinct leftmost and a rightmost derivations).  Of course, in
   principle, there isn't really a fixed ordering for the parse
   trees for a grammar: they could be listed in any order.  When we
   say "first n parse trees" here, we just mean the first n trees
   that are obtained using a breadth first search of a (sufficiently
   large) derivationTree.

   Example:

    Grammar> parseTrees 4 g1
    1) A    
            
    2)  A   
        |   
       .-.  
       | |  
       a A  
            
    3) A    
       |    
            
    4)  A   
        |   
       .--. 
       |  | 
       a  A 
          | 
         .-.
         | |
         a A
            
    
    Grammar>

----
parseTree n g  will print out the parse tree for item n in the
   list produced by parseTrees n g.  (We use a plural/singular
   naming convention for parseTrees/parseTree, as well as several
   other operations listed below, when the two functions are related
   in this way.)

   Example:

    Grammar> parseTree 4 g1
     A   
     |   
    .--. 
    |  | 
    a  A 
       | 
      .-.
      | |
      a A
    
    Grammar> 

----
langTrees n g  will print a list of the first n parse trees for
   strings in the language generated by the grammar (i.e., for
   strings with no nonterminals).

   Example:

    Grammar> langTrees 3 g1
    1) A    
       |    
            
    2)  A   
        |   
       .-.  
       | |  
       a A  
         |  
            
    3)  A   
        |   
       .--. 
       |  | 
       a  A 
          | 
         .-.
         | |
         a A
           |
            
    
    Grammar>

----
langTree n g  will print the nth tree in langTrees n g.

   Example:

    Grammar> langTree 4 g1
     A     
     |     
    .--.   
    |  |   
    a  A   
       |   
      .--. 
      |  | 
      a  A 
         | 
        .-.
        | |
        a A
          |

    Grammar>

----
sentences n g  will print a list of the first n sentences
   (terminals and nonterminals) that can be generated from g.

   Example:

    Grammar> sentences 8 g1
    1) A
    2) a A
    3) 
    4) a a A
    5) a
    6) a a a A
    7) a a
    8) a a a a A
    
    Grammar>

----
sentence n g  will print the nth sentence in sentences n g.

   Example:

    Grammar> sentence 4 g1
    a a A

    Grammar>

----
langSentences n g  will print a list of the first n sentences
   in the language defined by g (i.e., containing only terminal
   symbols).

   Example:

    Grammar> langSentences 8 g1
    1) 
    2) a
    3) a a
    4) a a a
    5) a a a a
    6) a a a a a
    7) a a a a a a
    8) a a a a a a a
    
    Grammar>

----
langSentence n g  will print the nth sentence in sentences n g.

   Example:

    Grammar> langSentence 5 g1
    a a a a

    Grammar>

----
sentenceDerivations n g  will print out the first n derivations
   for the grammar g.  Each derivation corresponds to a path in the
   derivationTree, starting at the root and descending to a child
   node at each step.  This function starts with the simple
   derivation at the root (requiring 0 steps), then continues to 1
   step derivations (the next level of the tree), then on to 2 step
   derivations, and so on.

   Example:

    Grammar> sentenceDerivations 4 g1
    1) A
    2) A  ==>  a A
    3) A  ==>  
    4) A  ==>  a A  ==>  a a A
    
    Grammar>

----
sentenceDerivation n g  will print the nth derivation for
   grammar g.

   Example:

    Grammar> sentenceDerivation 7 g1
    A  ==>  a A  ==>  a a A  ==>  a a

    Grammar>

----
derivationsToLevel n g  will print the derivations for the
   grammar g that have at most n steps (i.e., extending no further
   than the nth level of the derivationTree).

   Example:

    Grammar> derivationsToLevel 3 g1
    1) A
    2) A  ==>  a A
    3) A  ==>  
    4) A  ==>  a A  ==>  a a A
    5) A  ==>  a A  ==>  a
    6) A  ==>  a A  ==>  a a A  ==>  a a a A
    7) A  ==>  a A  ==>  a a A  ==>  a a

    Grammar>

----
treeDerivations n g  will print out the first n derivations
   for grammar g, showing the parse tree structure at each step.  In
   other respects, this function behaves much like
   sentenceDerivations described above.

   Example:

    Grammar> treeDerivations 4 g1
    1) A                          
                                  
    2) A   ==>    A               
                  |               
                 .-.              
                 | |              
                 a A              
                                  
    3) A   ==>   A                
                 |                
                                  
    4) A   ==>    A    ==>    A   
                  |           |   
                 .-.         .--. 
                 | |         |  | 
                 a A         a  A 
                                | 
                               .-.
                               | |
                               a A
    
    Grammar>

----
treeDerivation n g  will print the nth derivation for grammar
   g using parse trees to show the structure at each step.

   Example:

    Grammar> treeDerivation 7 g1
    A   ==>    A    ==>    A      ==>    A   
               |           |             |   
              .-.         .--.          .--. 
              | |         |  |          |  | 
              a A         a  A          a  A 
                             |             | 
                            .-.           .-.
                            | |           | |
                            a A           a A
                                            |
    
    Grammar> 

----
ambigExamples n g  will search the first n parse trees that are
   generated by g for examples of ambiguity (i.e., distinct parse
   trees that map to the same sentence).  Testing for ambiguity in a
   context free grammar is undecidable, so this function may not
   report any examples of ambiguity in an ambiguous grammar g if the
   value of n is too small.  On the other hand, if the value of n is
   too large, then this function may take a long time to execute (or
   may cause a stack or heap overflow if it runs out of memory).  In
   practice, you may want to start with relatively small values of n
   (a few tens, for example), and then increase or decrease that
   value on subsequent runs as you either expand or limit the
   search.  For each example of ambiguity that is found, the
   ambigExamples n g call will display the associated sentence as
   well as drawings for each of the possible parse tree structures.

   Example:

    Grammar> ambigExamples 10 g1
    No ambiguity examples found

    Grammar> ambigExamples 100 g1
    No ambiguity examples found

    Grammar> ambigExamples 10 "N -> a N ; N -> N a ; N -> a"
    1) a N a        
                    
        N        N  
        |        |  
       .--.     .--.
       |  |     |  |
       a  N     N  a
          |     |   
         .-.   .-.  
         | |   | |  
         N a   a N  
                    
    2) a a          
                    
        N     N     
        |     |     
       .-.   .-.    
       | |   | |    
       a N   N a    
         |   |      
         a   a      
                    
    Grammar>

   The first two examples here show that we were unable to find any
   examples of ambiguity in either the first 10 or first 100
   sentences derived for g1; this should not be too surprising
   because we know that g1 is unambiguous!  For the third example,
   we feed in an ambiguous grammar and find two supporting examples
   of that within the first 10 parse trees.  Note that the first
   example of ambiguity includes parse trees with a nonterminal N at
   a leaf node.  To find a concrete string in the language that
   illustrates ambiguity, we could further expand the corresponding
   nonterminals in each parse tree to an arbitrary string derived
   from that nonterminal.  But, of course, this wouldn't change the
   example in fundamental ways, and would just make the example look
   more complicated!


--------------------------------------------------------------------
The remainder of this file contains the implementations for the
functions described above.  As stated above, you are not expected
to read or understand the following code.  That said, you may find
it useful to add definitions for any grammars that you are working
with after the "import" declarations below (there are already
several other examples there to show you what this might look like).

> module GrammarToolkit where

> import Pic
> import Data.Char
> import Data.List

--------------------------------------------------------------------
Context Free Grammars:
----------------------
A simple abstract syntax for context free grammars:

> data Grammar  = Grammar [Prod]         -- grammar
>                 deriving Show

> type Prod     = (String, [Symbol]) -- production
> data Symbol   = NonTerm  String    -- nonterminal
>               | Terminal String    -- terminal
>                 deriving (Show, Eq)

Basic operators for extracting the productions and the start
symbol from a given grammar.

> prods             :: Grammar -> [Prod]
> prods (Grammar ps) = ps

> start             :: Grammar -> Symbol
> start              = NonTerm . fst . head . prods

> terminals         :: Grammar -> [String]
> terminals          = nub . concat . map terms . prods
>   where terms (n, syms) = [ s | (Terminal s) <- syms ]

> nonterminals      :: Grammar -> [String]
> nonterminals       = nub . concat . map nonterms . prods
>   where nonterms (n, syms) = n : [ s | (NonTerm s) <- syms ]

An operation to display a grammar:

> printGrammar :: Grammar -> IO ()
> printGrammar  = printList . map showProd . prods
>  where showProd         :: Prod -> String
>        showProd (n,syms) = n ++ " -> " ++ showSentence syms

A test to determine whether a given symbol is a terminal:

> isTerminal              :: Symbol -> Bool
> isTerminal (NonTerm n)   = False
> isTerminal (Terminal t)  = True

--------------------------------------------------------------------
A Simple Lexer and Parser for Grammars:
---------------------------------------
A lexical analyzer for grammars:

> data Token = TARROW     -- The arrow token
>            | TSEMI      -- Semicolon
>            | TT String  -- Terminal symbol
>            | TN String  -- Nonterminal symbol
>              deriving Show

> lexcfg             :: [Char] -> [Token]
> lexcfg []           = []
> lexcfg (';':cs)     = TSEMI  : lexcfg cs
> lexcfg ('-':'>':cs) = TARROW : lexcfg cs
> lexcfg (c:cs) 
>    | isSpace c   = lexcfg cs
>    | isUpper c   = lexNT (c:cs)
>    | otherwise   = lexTT (c:cs)

> lexNT   :: [Char] -> [Token]
> lexNT cs = case span isAlphaNum cs of
>              (nt, rest) -> TN nt : lexcfg rest

> lexTT   :: [Char] -> [Token]
> lexTT cs = case span (\c -> not (isSpace c || c==';')) cs of
>              (tt, rest) -> TT tt : lexcfg rest

A parser for grammars:

> parseprods          :: [Token] -> ([Prod], [Token])
> parseprods (TN n:ts) = case ts of
>                          (TARROW:ts1) ->
>                            case parsesymbols ts1 of
>                              (rhs, ts2) ->
>                                case ts2 of
>                                  (TSEMI:ts3) ->
>                                    case parseprods ts3 of
>                                      (prods,ts4) ->
>                                        ((n,rhs):prods, ts4)
>                                  ts5         -> ([(n,rhs)], ts5)
> parseprods ts        = ([], ts)

> parsesymbols          :: [Token] -> ([Symbol], [Token])
> parsesymbols (TT t:ts) = let (ss, ts1) = parsesymbols ts
>                          in (Terminal t:ss, ts1)
> parsesymbols (TN n:ts) = let (ss, ts1) = parsesymbols ts
>                          in (NonTerm n:ss, ts1)
> parsesymbols ts        = ([], ts)

Combining the above to implement syntax analysis for grammars:

> readGrammar  :: String -> Grammar
> readGrammar s = case parseprods (lexcfg s) of
>                   (ps, [] ) -> Grammar ps
>                   other     -> error "could not parse grammar"

If we further combine this with the printGrammar function, we get
an operation for pretty printing the parsed form of a grammar that
has been entered as a string.  This might, for example, be useful
as a way to confirm that the input string has been parsed correctly.

> grammar :: String -> IO ()
> grammar  = printGrammar . readGrammar

--------------------------------------------------------------------
Sentences:
----------
A Sentence is made up of a list of zero or more symbols:

> type Sentence = [Symbol]

We can view a single sentence as a string with the individual
words separated by spaces:

> showSentence :: Sentence -> String
> showSentence  = unwords . map str
>                 where str (NonTerm n)  = n
>                       str (Terminal t) = t

A string that is derived from the grammar and contains only
terminal symbols is said to be in the language for the grammar:

> inLanguage :: Sentence -> Bool
> inLanguage  = all isTerminal

--------------------------------------------------------------------
Sentence Trees:
---------------

> type SentenceTree = RoseTree Sentence

> leftmostTree  :: Grammar -> SentenceTree
> leftmostTree g = gentree leftmost [start g]
>  where leftmost             :: Sentence -> [Sentence]
>        leftmost (NonTerm n:syms)
>            | not (null rhss) = rhss
>              where rhss = [ rhs ++ syms | (m,rhs) <- prods g, m==n ]
>        leftmost (sym:syms) = [ sym : syms' | syms' <- leftmost syms ]
>        leftmost []         = []

> drawSentenceTree :: SentenceTree -> Pic
> drawSentenceTree  = drawRoseTree (\s -> " " ++ showSentence s ++ " ")

> leftMostDerivationTree  :: Int -> String -> Pic
> leftMostDerivationTree n = drawSentenceTree
>                          . prune n
>                          . leftmostTree
>                          . readGrammar

> sentenceTree :: Grammar -> SentenceTree 
> sentenceTree g = gentree expand [start g]
>  where expand :: Sentence -> [Sentence]
>        expand xs = [ ls ++ syms ++ rs | (ls, NonTerm n, rs) <- splits xs,
>                                         (m, syms) <- prods g,
>                                         m == n ]

--------------------------------------------------------------------
Parse Trees:
------------
The nodes in a parse tree are either leaf nodes corresponding to
individual symbols or else an interior node corresponding to a
production in the grammar.  The parse trees for strings in the
language defined by the grammar do not have any leaf nodes with
nonterminal symbols.

> data ParseTree = Sym Symbol
>                | Prod String [ParseTree]
>                  deriving (Eq)

We can recover the sentence corresponding to any given parse tree
by using the following flatten operator:

> flatten            :: ParseTree -> Sentence
> flatten (Sym s)     = [s]
> flatten (Prod n ts) = concat (map flatten ts)

For the purposes of visualization, we define a function for drawing
a picture of an arbitrary parse tree:

> drawParseTree :: ParseTree -> Pic
> drawParseTree  = fst . adraw
>  where adraw (Sym (NonTerm n)) = centerstring n
>        adraw (Sym (Terminal t)) = centerstring t
>        adraw (Prod s ts)
>          = centerstring s
>            `above` centerstring "|"
>            `above` case map adraw ts of
>              []  -> (empty, 0)
>              [t] -> t
>              ts  -> spanchildren ts

--------------------------------------------------------------------
ParseTree Trees:
----------------

> type ParseTreeTree = RoseTree ParseTree

> allParseTrees  :: Grammar -> ParseTreeTree
> allParseTrees g = gentree expand (Sym (start g))
>  where
>   expand (Sym (Terminal t))
>     = []
>   expand (Sym (NonTerm n))
>     = [ Prod n (map Sym rhs) | (m, rhs) <- prods g, m == n ]
>   expand (Prod n ts)
>     = [ Prod n (ls++[t']++rs) | (ls, t, rs) <- splits ts,
>                                 t'          <- expand t ]

> splits       :: [a] -> [([a], a, [a])]
> splits []     = []
> splits (x:xs) = ([], x, xs) : [ (x:us,v,ws) | (us, v, ws) <- splits xs ]

Functions for displaying some initial (likely small) number of rows of a
ParseTreeTree, capturing the set of all possible derivations from the
starting symbol of a grammar:

> drawParseTreeTree :: ParseTreeTree -> Pic
> drawParseTreeTree  = drawSentenceTree . maptree flatten

> derivationTree    :: Int -> String -> Pic
> derivationTree n   = drawParseTreeTree
>                    . prune n
>                    . allParseTrees
>                    . readGrammar

Functions for displaying parse trees from a grammar:

> allTrees     :: String -> [ParseTree]
> allTrees      = bfs . allParseTrees . readGrammar

> parseTrees   :: Int -> String -> Pic
> parseTrees n  = drawList
>               . map drawParseTree
>               . take n
>               . allTrees

> parseTree    :: Int -> String -> Pic
> parseTree  n  = drawParseTree
>               . pick n
>               . allTrees

Functions for displaying parse trees corresponding to strings
in the language for the grammar:

> allLangTrees :: String -> [ParseTree]
> allLangTrees  = filter (inLanguage . flatten) . allTrees

> langTrees    :: Int -> String -> Pic
> langTrees n   = drawList
>               . map drawParseTree
>               . take n
>               . allLangTrees

> langTree     :: Int -> String -> Pic
> langTree  n   = drawParseTree
>               . pick n
>               . allLangTrees

Functions for displaying sentences from a grammar:

> allSentences :: String -> [Sentence]
> allSentences  = map flatten . allTrees

> sentences    :: Int -> String -> IO ()
> sentences n   = printList
>               . map showSentence
>               . take n
>               . allSentences

> sentence     :: Int -> String -> IO ()
> sentence  n   = putStrLn
>               . showSentence
>               . pick n
>               . allSentences

Functions for displaying sentences corresponding to strings in the
language:

> allLangSentences :: String -> [Sentence]
> allLangSentences  = filter inLanguage . allSentences

> langSentences    :: Int -> String -> IO ()
> langSentences n   = printList
>                   . map showSentence
>                   . take n
>                   . allLangSentences

> langSentence     :: Int -> String -> IO ()
> langSentence n    = putStrLn
>                   . showSentence
>                   . pick n
>                   . allLangSentences

--------------------------------------------------------------------
Derivations:
------------
A derivation is a sequence of items in which each element, other
that the initial item, is derived from its predecessor by replacing
a nonterminal with the right hand side of a corresponding production.

> type Derivation a = [a]

> allDerivations       :: String -> [Derivation ParseTree]
> allDerivations        = map reverse
>                       . bfs
>                       . pathtree
>                       . allParseTrees
>                       . readGrammar

> allLangDerivations   :: String -> [Derivation ParseTree]
> allLangDerivations    = filter (inLanguage . flatten . last)
>                       . allDerivations

Functions for printing out derivations using sentences:

> sentenceDerivations  :: Int -> String -> IO ()
> sentenceDerivations n = printList
>                       . map showDerivation
>                       . take n
>                       . allDerivations

> sentenceDerivation n  = putStrLn
>                       . showDerivation
>                       . pick n
>                       . allDerivations

> derivationsToLevel   :: Int -> String -> IO ()
> derivationsToLevel n  = printList
>                       . map (showDerivation . reverse)
>                       . bfs
>                       . prune n
>                       . pathtree
>                       . allParseTrees
>                       . readGrammar

> showDerivation       :: Derivation ParseTree -> String
> showDerivation        = concat
>                       . intersperse "  ==>  "
>                       . map (showSentence . flatten)

Finding derivations for sentences in the language:

> langDerivations      :: Int -> String -> IO ()
> langDerivations n     = printList
>                       . map showDerivation
>                       . take n
>                       . allLangDerivations

> langDerivation       :: Int -> String -> IO ()
> langDerivation n      = putStrLn
>                       . showDerivation
>                       . pick n
>                       . allLangDerivations

Functions for printing out derivations using parse trees:

> treeDerivations      :: Int -> String -> Pic
> treeDerivations n     = drawList
>                       . map drawDerivation
>                       . take n
>                       . allDerivations

> treeDerivation       :: Int -> String -> Pic
> treeDerivation n      = drawDerivation
>                       . pick n
>                       . allDerivations

> treeLangDerivations  :: Int -> String -> Pic
> treeLangDerivations n = drawList
>                       . map drawDerivation
>                       . take n
>                       . allLangDerivations

> treeLangDerivation   :: Int -> String -> Pic
> treeLangDerivation n  = drawDerivation
>                       . pick n
>                       . allLangDerivations

> drawDerivation       :: Derivation ParseTree -> Pic
> drawDerivation        = pack 2 72
>                       . intersperse (string " ==> ")
>                       . map drawParseTree

--------------------------------------------------------------------
Searching for Ambiguity:
------------------------

> findAmbig   :: [(Sentence, ParseTree)] -> [(Sentence, [ParseTree])]
> findAmbig [] = []
> findAmbig ((s,t):sts)
>              = case partition ((s==) . fst) sts of
>                  ([], rest) -> findAmbig rest
>                  (as, rest) -> (s, t : map snd as) : findAmbig rest

> drawAmbiguous [] = string "No ambiguity examples found"
> drawAmbiguous as = drawList (map drawAmbig as)
>  where drawAmbig (s, ts) = string (showSentence s)
>                            `left` vstrut 1 `left`
>                            foldr1 (\l r -> l `top` hstrut 3 `top` r)
>                                   (map drawParseTree ts)

> ambigExamples  :: Int -> String -> Pic
> ambigExamples n = drawAmbiguous
>                 . findAmbig
>                 . map (\t -> (flatten t, t))
>                 . nub
>                 . take n
>                 . allTrees

--------------------------------------------------------------------
General Utilities:
------------------

> printList    = putStr . unlines . zipWith join [1..]
>                where join i s = show i ++ ") " ++ s

> drawList     = foldr above empty . zipWith join [1..]
>                where join i p  = string (show i ++ ") ") `top` p
>                      above p q = p `left` vstrut 1 `left` q

> pick        :: Int -> [a] -> a
> pick n xs    = xs !! (n-1)

--------------------------------------------------------------------
Rose Trees
----------
We define a "RoseTree" data structure that is a general form of tree
structure with a value and a list of subtrees stored at each node.

> data RoseTree a = Node a [RoseTree a]

> sampleRoseTree :: RoseTree Int
> sampleRoseTree  = Node 1 [ Node 2 [ Node 3 [], Node 4 [] ],
>                            Node 5 [],
>                            Node 6 [ Node 7 [ Node 8 [] ],
>                                     Node 9 [] ] ]

> gentree  :: (a -> [a]) -> a -> RoseTree a
> gentree step = tree
>  where tree syms = Node syms (map tree (step syms))

> maptree              :: (a -> b) -> RoseTree a -> RoseTree b
> maptree f (Node a ts) = Node (f a) (map (maptree f) ts)

> bfs :: RoseTree a -> [a]
> bfs  = concat . levels
>  where levels (Node s ts) = [s] : map concat (transpose (map levels ts))

> prune              :: Int -> RoseTree a -> RoseTree a
> prune 0 (Node s ts) = Node s []
> prune n (Node s ts) = Node s (map (prune (n-1)) ts)

> pathtree :: RoseTree a -> RoseTree [a]
> pathtree = paths []
>  where paths as (Node a ts) = Node as' (map (paths as') ts)
>                               where as' = a : as

> drawRoseTree      :: (a -> String) -> RoseTree a -> Pic
> drawRoseTree label = fst . adrawRoseTree label
>  where adrawRoseTree label (Node s ts)
>          = let lpic = centerstring (label s)
>                vbar = centerstring "|"
>            in case map (adrawRoseTree label) ts of
>              []  -> lpic
>              [t] -> lpic `above` vbar `above` t
>              ts  -> lpic `above` vbar `above` spanchildren ts

--------------------------------------------------------------------
