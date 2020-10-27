module Ex2 where

import Log

insert :: Log.LogMessage -> Log.MessageTree -> Log.MessageTree
insert (Log.Unknown _) tree = tree
insert lm Log.Leaf          = Log.Node Log.Leaf lm Log.Leaf
insert (Log.LogMessage mt ts s) (Log.Node ltree (Log.LogMessage cur_mt cur_ts cur_s) rtree)
    | ts < cur_ts = Log.Node (insert lm ltree) cur_lm rtree
    | otherwise   = Log.Node ltree cur_lm (insert lm rtree)
    where lm     = (Log.LogMessage mt ts s)
          cur_lm = (Log.LogMessage cur_mt cur_ts cur_s)