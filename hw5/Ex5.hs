{-# LANGUAGE FlexibleInstances #-}

module Ex5 where

import Parser
import StackVM
import Ex3

instance Expr Program where
    lit x = [PushI x]
    add x y = x ++ y ++ [Add]
    mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile s = parseExp lit add mul s