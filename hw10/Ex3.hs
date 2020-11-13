module Ex3 where

import AParser
import Ex2

abParser :: Parser (Char, Char)
abParser = pure (,) <*> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = pure (\(x,y) -> ()) <*> abParser

intPair :: Parser [Integer]
intPair = pure (\x y z -> [x, z]) <*> posInt <*> char ' ' <*> posInt