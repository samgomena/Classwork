> module ParseInfix where

> import Lex
> import Prop
> import Pic

> ppic = picTree . fst . parseP . lexProp

P -> C OR P
P -> C

> parseP :: [Token] -> (Prop, [Token])
> parseP ts = case parseC ts of
>               (l, TOR:ts1) -> case parseP ts1 of
>                                   (r, ts2) -> (OR l r, ts2)
>               (l, ts1) -> (l, ts1)

C -> N AND C
C -> N

> parseC :: [Token] -> (Prop, [Token])
> parseC ts = case parseN ts of
>               (l, TAND:ts1) -> case parseC ts1 of
>                                   (r, ts2) -> (AND l r, ts2)
>               (l, ts1) -> (l, ts1)

N -> NOT N
N -> A

> parseN :: [Token] -> (Prop, [Token])
> parseN (TNOT : ts) = case parseN ts of
>                           (p, ts1) -> (NOT p, ts1)
> parseN ts = parseA ts

A -> TRUE
A -> FALSE
A -> VAR
A -> ( P )

> parseA :: [Token] -> (Prop, [Token])
> parseA (TVAR s : ts) = (VAR s, ts)
> parseA (TOPEN : ts) = case parseP ts of
>                           (p, TCLOSE : ts1) -> (p, ts1)
>                           (p, ts1) -> error "close paren"
> parseA ts1 = error "cannot parse atom"

> {-
> parseProp :: [Token] -> (Prop, [Token])

> parseProp (TVAR word : ts)
>   = (VAR word, ts)

> parseProp (TNOT : ts)
>   = case parseProp ts of
>       (p, ts1) -> (NOT p, ts1)

> parseProp (TAND : ts)
>   = case parseProp ts of
>       (p, ts1) -> case parseProp ts1 of
>           (q, ts2) -> (AND p q, ts2)

> parseProp (TOR : ts)
>   = case parseProp ts of
>       (p, ts1) -> case parseProp ts1 of
>           (q, ts2) -> (OR p q, ts2)

> parseProp (TOPEN : ts)
>   = case parseProp ts of
>       (p, TCLOSE : ts1) -> (p, ts1)
>       (p, ts2) -> error "missing close parens"

> -}


