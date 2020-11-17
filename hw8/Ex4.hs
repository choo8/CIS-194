module Ex4 where

import Employee
import Data.Tree
import Ex1
import Ex2
import Ex3

maxFun :: Tree Employee -> GuestList
maxFun t = moreFun with without
    where (with, without) = foldTree nextLevel t