{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Ex6 where

import Ex3
import Ex4

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger n = Cons n (streamRepeat 0)
    negate as = streamMap (* (-1)) as
    (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
    (Cons a as) * (Cons b bs) = Cons (a * b) ((streamMap (*a) bs) + (as * (Cons b bs)))

instance Fractional (Stream Integer) where
    (Cons a as) / (Cons b bs) = Cons (div a b) (streamMap (\x -> div x b) (as - (((Cons a as) / (Cons b bs)) * bs)))

fibs3 :: Stream Integer
fibs3 = x / ((fromInteger 1) - x - x^2)