module Ex1 where

everyN :: (Int, [a]) -> [a]
everyN (n, xs)
    | null left = []
    | otherwise = head left : everyN (n, (tail left))
    where left = drop (n - 1) xs

skips :: [a] -> [[a]]
skips xs = map everyN (zip [1..n] (replicate n xs))
    where n = length xs