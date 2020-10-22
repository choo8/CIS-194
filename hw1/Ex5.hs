module Ex5 where

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 x y z = [(x, y)]
hanoi n x y z = (hanoi (n - 1) x z y) ++ [(x, y)] ++ (hanoi (n - 1) z y x)