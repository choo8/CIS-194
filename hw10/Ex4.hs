module Ex4 where

import AParser
import Ex1
import Ex2

class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a

instance Alternative Parser where
    empty = Parser (\x -> Nothing)
    Parser f <|> Parser g = Parser (\x -> let res = (f x) in if isNothing res then (g x) else res)