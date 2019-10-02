--------------------------------------------------------------------

> module GrammarScratch where
> import GrammarToolkit

My sample grammars

> eg = "E -> n ; E -> E + E ; E -> E * E ; E -> ( E )"
> unambig = "E -> E + P ; E -> P ; " ++
>           "P -> P * A ; P -> A ; " ++
>           "A -> n ; A -> ( E )"

> two_a = "L -> n T ; T -> , L ; T ->"
> two_e = "A -> ; A -> 0B ; A -> 3B ; A -> 6B ; A -> 9B ; B -> 1B ; B -> 4B ; B -> 7B ; B -> 2B ; B -> 5B ; B -> 7B ; B -> A"
> t_b = "E -> P|E ; E -> S|E ; E -> R|E ; E -> P ; E -> S ; E -> R ; P -> ( E ) ; R -> P* ; R -> C* ; S -> SC ; S -> C ; C -> c ; C ->"

Grammars from the lab

> dangling_else = "S -> if e then S else S ; S -> if e then S ; S -> other"
> non_dangling_else = "S -> if e then S else S ; S -> if e then S| ; S -> other"

Some sample grammars:

> g1 = "A -> a A ; A ->"
> g2 = "E -> num ; E -> E + E; E -> - E ; E -> ( E )"
> g3 = "P -> true ; P -> not P ; P -> and P P"
> g4 = "P -> true ; P -> false; P -> not P ; P -> P and P"
> g5 = "P -> P & N; P -> N; N -> ~ N; N -> t"
> g6 = "P -> N & P; P -> N; N -> ~ N; N -> t"
> g7 = "L -> x ; L -> L L ; L -> \\ x . L ; L -> ( L )"

A grammar for a language that is not regular:

> brackets = "B -> ; B -> [ B ]"

--------------------------------------------------------------------

The GrammarToolkit defines types Grammar, Prod, Symbol for
representing grammars (no surprise!), productions, and symbols
(which includes both terminals and nonterminals).  It also
provides the following operations:

* start        :: Grammar -> Symbol
  prods        :: Grammar -> [Prod]
  terminals    :: Grammar -> [String]
  nonterminals :: Grammar -> [String]

  Access the components of a grammar.

* readGrammar :: String -> Grammar

  Construct a grammar from a string.

* grammar      :: String -> IO ()
  printGrammar :: Grammar -> IO ()

  Display a grammar.

* derivationTree :: Int -> String -> Pic

  Display a tree with all derivations  

* sentences :: Int -> String -> IO ()
  sentence  :: Int -> String -> IO ()

  Display derivable sentences.

* langSentences :: Int -> String -> IO ()
  langSentence  :: Int -> String -> IO ()

  Display derivable sentences in the language (terminals only).

* parseTrees :: Int -> String -> Pic
  parseTree  :: Int -> String -> Pic

  Display derivable parse trees.

* langTrees :: Int -> String -> Pic
  langTree  :: Int -> String -> Pic

  Display derivable parse trees in the language (terminals only).

* sentenceDerivations :: Int -> String -> IO ()
  sentenceDerivation  :: Int -> String -> IO ()

  Display derivations for all sentences.

* derivationsToLevel :: Int -> String -> IO ()

  Display all the derivations up to length n.

* langDerivations :: Int -> String -> IO ()
  langDerivation  :: Int -> String -> IO ()

  Display derivations for sentences in the language (terminals only).

* treeDerivations :: Int -> String -> Pic
  treeDerivation  :: Int -> String -> Pic

  Display derivations with parse trees.

* treeLangDerivations :: Int -> String -> Pic
  treeLangDerivation  :: Int -> String -> Pic

  Display derivations with parse trees for sentences that only
  contain terminal symbols.

* ambigExamples :: Int -> String -> Pic

  Search for examples of ambiguity in the first n derivations from
  the grammar.

--------------------------------------------------------------------
