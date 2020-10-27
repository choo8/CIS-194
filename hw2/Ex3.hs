module Ex3 where

import Log
import Ex2

build :: [Log.LogMessage] -> Log.MessageTree
build lms = foldr Ex2.insert Log.Leaf lms