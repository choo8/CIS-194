module Ex3 where

xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then ((x && (not y)) || ((not x) && y)) else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x) : y) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x y -> f y x) base (reverse xs)
