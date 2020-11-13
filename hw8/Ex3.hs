module Ex3 where

import Employee
import Ex1

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp xs = (glCons emp glwithout, glwith)
    where glwith = mconcat . fmap fst $ xs
          glwithout = mconcat . fmap snd $ xs