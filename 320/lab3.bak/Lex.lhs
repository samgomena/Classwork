> module Lex where
> import Data.Char

> data Token = TAND | TOR | TNOT
>           | TOPEN | TCLOSE
>           | TVAR String
>             deriving Show

> lexProp :: [Char] -> [Token]
> lexProp [] = []
> lexProp ('(':cs) = TOPEN : lexProp cs
> lexProp (')':cs) = TCLOSE : lexProp cs
> lexProp (c:cs)
>       | isSpace c = lexProp cs
>       | isAlpha c = lexWord (c:cs)
>       | otherwise = error "unexpected character"

> lexWord :: [Char] -> [Token]
> lexWord cs = case span isAlpha cs of
>               ("NOT", rest) -> TNOT : lexProp rest
>               ("AND", rest) -> TAND : lexProp rest
>               ("OR", rest) -> TOR : lexProp rest
>               (word, rest) -> TVAR word : lexProp rest
