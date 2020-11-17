module Ex2 where

import Data.Tree
import Employee
import Ex1

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) . subForest $ t)