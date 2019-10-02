> module Parse where

> import Lex
> import Prop

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






