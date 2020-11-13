module Ex5 where

import AParser
import Data.Char
import Ex1
import Ex2
import Ex3
import Ex4

intOrUppercase :: Parser ()
intOrUppercase = (pure (\x -> ()) <*> posInt) <|> (pure (\x -> ()) <*> (satisfy isUpper))