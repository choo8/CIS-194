module Ex2 where

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = fst $ foldr (\x (acc, bool) -> if bool 
                                                        then ((2 * x) : acc, not bool) 
                                                        else (x : acc, not bool)) ([], False) xs