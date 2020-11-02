{-# LANGUAGE FlexibleInstances #-}

module Ex6 where

import Data.Maybe
import qualified Data.Map as M
import Ex3

class HasVars a where
    var :: String -> a

data VarExprT = Var String
           | Lit Integer
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
  deriving (Show, Eq)

instance Expr VarExprT where
    lit x = Lit x
    add x y = Add x y
    mul x y = Mul x y

instance HasVars VarExprT where
    var s = Var s

instance HasVars (M.Map String Integer -> Maybe Integer) where
    var s = (\m -> M.lookup s m)

instance Expr (M.Map String Integer -> Maybe Integer) where
    lit x = (\m -> Just x)
    add x y = (\m -> case (isNothing (x m) || isNothing (y m)) of
                        True -> Nothing
                        _ -> Just (fromJust (x m) + fromJust (y m)))
    mul x y = (\m -> case (isNothing (x m) || isNothing (y m)) of
                        True -> Nothing
                        _ -> Just (fromJust (x m) * fromJust (y m)))