module Ex2 where

import AParser
import Ex1

instance Applicative Parser where
    pure x = Parser (\y -> Just (x, y))
    Parser f <*> Parser g = Parser (\x -> if isNothing (f x) || isNothing (g (snd . fromJust . f $ x)) 
                                          then Nothing                                                                                                          
                                          else Just . first (fst . fromJust . f $ x) . fromJust . g $ (snd . fromJust . f $ x))