module Ex2 where

makeTriples :: [a] -> [[a]]
makeTriples (x:y:z:xs) = [x, y, z] : makeTriples (y : z : xs)
makeTriples [] = []
makeTriples (_) = []

isMaxima :: (Ord a) => [a] -> Bool
isMaxima (x:y:z:xs) = (x<y) && (y>z) && (null xs)
isMaxima _ = False

getMiddle :: [a] -> a
getMiddle (x:y:z) = y

localMaxima :: [Integer] -> [Integer]
localMaxima = map getMiddle . filter isMaxima . makeTriples