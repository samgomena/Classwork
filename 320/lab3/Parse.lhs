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

parseDefns :: [Token] -> ([String, Prop], [Token])

 parseDefns ts = case parseProp ts of 
                       (var, TEQUAL:ts1) -> ((var, fst (parseProp ts1)), ts)

parseDefns (TVAR word : TEQUAL : ts) = error "missing semicolon"

parseDefns (TVAR word : TEQUAL : ts) = ((word, fst (parseProp ts)), ts) 

 parseDefns [] = ([], [])

Defs -> Def ";" Defs
Defs -> 

Def -> name "=" Prop

> parseDefns (TVAR word : TEQUAL : ts)
>       = case parseDefn ts of
>               (p, TTERM : ts1) -> case parseDefns ts1 of
>                   (q, ts2) -> ((word, p), ts2)


> parseDefn :: [Token] -> (Prop, [Token])
> parseDefn (TNOT : ts)
>   = case parseDefn ts of
>       (p, ts1) -> (NOT p, ts1)

> parseDefn (TAND : ts)
>   = case parseDefn ts of
>       (p, ts1) -> case parseDefn ts1 of
>           (q, ts2) -> (AND p q, ts2)

> parseDefn (TOR : ts)
>   = case parseDefn ts of
>       (p, ts1) -> case parseDefn ts1 of
>           (q, ts2) -> (OR p q, ts2)

> parseDefn (TOPEN : ts)
>   = case parseDefn ts of 
>       (p, TCLOSE : ts1) -> (p, ts1)
>       (p, ts2) -> error "missing close parens"

> parseDefn (TVAR word : ts) = (VAR word, ts)


