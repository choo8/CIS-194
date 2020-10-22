module Ex1 where

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n == 0 || n < 0 = []
    | x == n          = [x]
    | otherwise       = x : toDigitsRev (n `quot` 10) 
    where x = n `mod` 10