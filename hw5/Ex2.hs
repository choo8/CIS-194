module Ex2 where

import ExprT
import Parser
import Ex1

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit Add Mul s of
                Just exp -> Just (eval exp)
                Nothing -> Nothing