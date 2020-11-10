{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Ex7 where

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
    (Matrix x11 x12 x21 x22) * (Matrix y11 y12 y21 y22) = (Matrix (x11 * y11 + x12 * y21) (x11 * y12 + x12 * y22) (x21 * y11 + x22 * y21) (x21 * y12 + x22 * y22))

getFn :: Matrix -> Integer
getFn (Matrix _ fn _ _) = fn

fibs4 :: Integer -> Integer
fibs4 n
    | n == 0    = 0
    | otherwise = getFn m
    where m = (Matrix 1 1 1 0) ^ n