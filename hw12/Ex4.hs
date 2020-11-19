module Ex4 where

import Risk
import Ex2
import Ex3
import Control.Monad.Random

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= (\xs -> let res = map (\x -> (defenders x) == 0) xs in return ((sum . map (\y -> if y then 1 else 0) $ res) / 1000.0))