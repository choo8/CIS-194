module Ex4 where

import Log

inOrder :: Log.MessageTree -> [Log.LogMessage]
inOrder (Log.Leaf) = []
inOrder (Log.Node ltree lm rtree) = (inOrder ltree) ++ [lm] ++ (inOrder rtree) 