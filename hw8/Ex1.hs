{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ex1 where

import Employee

instance Semigroup GuestList where
    (GL xs f1) <> (GL ys f2) = (GL (xs ++ ys) (f1 + f2)) 

instance Monoid GuestList where
    mempty = (GL [] 0)

glCons :: Employee -> GuestList -> GuestList
glCons x (GL xs f) = (GL (x:xs) (f + empFun x))

moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL xs f1) (GL ys f2)
    | f1 >= f2  = (GL xs f1)
    | otherwise = (GL ys f2)