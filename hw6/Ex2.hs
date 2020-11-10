module Ex2 where

memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)

fibs2 :: [Integer]
fibs2 = map memoized_fib [0..]