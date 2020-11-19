module Ex3 where

import Risk
import Ex2
import Control.Monad.Random

isInvasionOver :: Battlefield -> Bool
isInvasionOver bf
    | (nd == 0) || (na < 2) = True
    | otherwise             = False
    where na = attackers bf
          nd = defenders bf

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = if isInvasionOver bf then return bf else battle bf >>= invade