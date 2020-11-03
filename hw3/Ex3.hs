module Ex3 where

addOneHot :: [Integer] -> Integer -> [Integer]
addOneHot (x:xs) n = if n == 0 then (x + 1) : xs else x : addOneHot xs (n - 1)

countFrequency :: [Integer] -> [Integer]
countFrequency xs = foldl addOneHot (take 10 (repeat 0)) xs

histogram :: [Integer] -> String
histogram xs = (foldl (\acc x -> acc ++ (map (\y -> if y >= x then '*' else ' ') freqs) ++ "\n") [] (reverse [1..highest])) ++ "==========\n0123456789\n"
    where freqs   = countFrequency xs
          highest = maximum freqs