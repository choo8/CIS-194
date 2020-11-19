-- Recursive algorithm referenced from https://blog.wolfram.com/2017/11/20/how-to-win-at-risk-exact-probabilities/

module Ex5 where

import Risk
import Data.List

probWin2 :: Battlefield -> Double
probWin2 bf
    | na > 3 = (fromIntegral (length [(v,w,x,y,z) | (v,w,x,y,z) <- fiveDie, (maximum [v,w,x]) > (max y z), ((sort [v,w,x]) !! 1) > (min y z)])) / (fromIntegral (length fiveDie))
    | otherwise = (fromIntegral (length [(w,x,y,z) | (w,x,y,z) <- fourDie, (max w x) > (max y z), (min w x) > (min y z)])) / (fromIntegral (length fourDie))
    where na = attackers bf
          nd = defenders bf
          fourDie = [(w,x,y,z) | w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]
          fiveDie = [(v,w,x,y,z) | v <- [1..6], w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]

probWin1Lose1 :: Battlefield -> Double
probWin1Lose1 bf = 1 - (probWin2 bf) - (probLose2 bf)

probLose2 :: Battlefield -> Double
probLose2 bf 
    | na > 3 = (fromIntegral (length [(v,w,x,y,z) | (v,w,x,y,z) <- fiveDie, (maximum [v,w,x]) <= (max y z), ((sort [v,w,x]) !! 1) <= (min y z)])) / (fromIntegral (length fiveDie))
    | otherwise = (fromIntegral (length [(w,x,y,z) | (w,x,y,z) <- fourDie, (max w x) <= (max y z), (min w x) <= (min y z)])) / (fromIntegral (length fourDie))
    where na = attackers bf
          nd = defenders bf
          fourDie = [(w,x,y,z) | w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]
          fiveDie = [(v,w,x,y,z) | v <- [1..6], w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]

probWin1 :: Battlefield -> Double
probWin1 bf
    | na > 3 && nd == 1 = (fromIntegral (length [(w,x,y,z) | (w,x,y,z) <- fourDie, (maximum [w,x,y]) > z])) / (fromIntegral (length fourDie))
    | na == 3 && nd == 1 = (fromIntegral (length [(x,y,z) | (x,y,z) <- threeDie, (max x y) > z])) / (fromIntegral (length threeDie))
    | na == 2 && nd >= 2 = (fromIntegral (length [(x,y,z) | (x,y,z) <- threeDie, x > (max y z)])) / (fromIntegral (length threeDie))
    | otherwise = (fromIntegral (length [(x,y) | (x,y) <- twoDie, x > y])) / (fromIntegral (length twoDie))
    where na = attackers bf
          nd = defenders bf
          twoDie = [(x,y) | x <- [1..6], y <- [1..6]]
          threeDie = [(x,y,z) | x <- [1..6], y <- [1..6], z <- [1..6]]
          fourDie = [(w,x,y,z) | w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]

probLose1 :: Battlefield -> Double
probLose1 bf = 1 - (probWin1 bf)

exactSuccessProb :: Battlefield -> Double
exactSuccessProb bf
    | na >= 3 && nd >= 2 = ((probWin2 (Battlefield na nd)) * (exactSuccessProb (Battlefield na (nd - 2)))) + ((probWin1Lose1 (Battlefield na nd)) * (exactSuccessProb (Battlefield (na - 1) (nd - 1)))) + ((probLose2 (Battlefield na nd)) * (exactSuccessProb (Battlefield (na - 2) nd)))
    | na > 1 && nd >= 1 = ((probWin1 (Battlefield na nd)) * (exactSuccessProb (Battlefield na (nd - 1)))) + ((probLose1 (Battlefield na nd)) * (exactSuccessProb (Battlefield (na - 1) nd)))
    | na == 1 = 0
    | nd == 0 = 1
    where na = attackers bf
          nd = defenders bf