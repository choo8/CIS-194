module Ex1 where

import Log

parseMessage :: String -> Log.LogMessage
parseMessage s
    | mt == "I" = Log.LogMessage Log.Info (read (mlist !! 1)) (unwords (drop 2 mlist))
    | mt == "W" = Log.LogMessage Log.Warning (read (mlist !! 1)) (unwords (drop 2 mlist))
    | mt == "E" = Log.LogMessage (Log.Error (read (mlist !! 1))) (read (mlist !! 2)) (unwords (drop 3 mlist))
    | otherwise = Log.Unknown s  
    where mlist = words s 
          mt    = head mlist

parse :: String -> [Log.LogMessage]
parse f = map parseMessage logs
    where logs = lines f