module Ex2 where

import Risk
import Data.List
import Control.Monad.Random

dieVals :: Army -> Rand StdGen [DieValue]
dieVals n = do
    sequence . take n . repeat $ die

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
    as <- dieVals na
    ds <- dieVals nd
    let res = zipWith (-) (reverse . sort . map unDV $ as) (reverse . sort . map unDV $ ds)
        naDead = length . filter (<=0) $ res
        ndDead = length . filter (>0) $ res
    return (Battlefield ((attackers bf) - naDead) ((defenders bf) - ndDead))
    where na = min 3 ((attackers bf) - 1)
          nd = min 2 (defenders bf)