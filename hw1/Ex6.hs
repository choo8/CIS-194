-- Frame-Stewart Algorithm

import Ex5

hanoi4 :: Integer -> Ex5.Peg -> Ex5.Peg -> Ex5.Peg -> Ex5.Peg -> [Ex5.Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n w x y z = (hanoi4 k w y x z) ++ (Ex5.hanoi (n - k) w x y) ++ (hanoi4 k z x w z)
    where k = n - (round (sqrt (2 * (fromIntegral n) + 1))) + 1