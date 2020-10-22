module Ex3 where

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (\acc x -> (x `quot` 10) + (x `mod` 10) + acc) 0 xs