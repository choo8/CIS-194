module Ex1 where

import AParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x,y)

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing (Just x) = False

fromJust :: Maybe a -> a
fromJust (Just x) = x

instance Functor Parser where
    fmap f (Parser g) = (Parser (\x -> if isNothing . g $ x then Nothing else Just . first f . fromJust . g $ x))