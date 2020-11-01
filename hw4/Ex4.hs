module Ex4 where

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

excluded n = map (\(x, y) -> x + y + (2 * x * y)) $ cartProd [1..n] [1..n]

sieveSundaram :: Integer -> [Integer]
sieveSundaram = nums
    where nums n = [2*x + 1 | x <- [1..n], not (elem x (excluded n))]
