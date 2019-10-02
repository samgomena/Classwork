> module SimpProp(simplify) where

This file provides an implementation of a function that
will attempt to simplify an arbitrary Prop circuit by
using standard laws of Boolean algebra.  This function
can be thought of as a simple example of a program
translator (as a kind of optimizer or compiler, for example),
but it is only possible to use a function like this if we
are able to view Prop programs as structured data ...

> import Prop

> simplify          :: Prop -> Prop
> simplify (NOT p)   = simpNOT (simplify p)
> simplify (AND p q) = simpAND (simplify p) (simplify q)
> simplify (OR p q)  = simpOR  (simplify p) (simplify q)
> simplify p         = p

The main simplify function is defined in terms of three
auxiliary functions that capture the rules for simplifying
NOT, AND, and OR, respectively:

> simpNOT                      :: Prop -> Prop
> simpNOT TRUE                  = FALSE
> simpNOT FALSE                 = TRUE
> simpNOT (AND (NOT p) (NOT q)) = OR p q
> simpNOT (OR (NOT p) (NOT q))  = AND p q
> simpNOT (NOT p)               = p
> simpNOT p                     = NOT p

> simpAND        :: Prop -> Prop -> Prop
> simpAND TRUE  q = q
> simpAND FALSE q = FALSE
> simpAND p TRUE  = p
> simpAND q FALSE = FALSE
> simpAND p q     = if p==q then p else AND p q

> simpOR        :: Prop -> Prop -> Prop
> simpOR TRUE  q = TRUE
> simpOR FALSE q = q
> simpOR p TRUE  = TRUE
> simpOR q FALSE = q
> simpOR p q     = if p==q then p else OR p q

