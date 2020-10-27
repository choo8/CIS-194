module Ex5 where

import Log
import Data.List

isErrorAbove :: Int -> Log.LogMessage -> Bool
isErrorAbove n (Log.LogMessage (Log.Error lvl) _ _)
    | lvl > n   = True
    | otherwise = False
isErrorAbove _ _ = False 

getString :: Log.LogMessage -> String
getString (Log.LogMessage (Log.Error _) _ s) = s 

getTimeStamp :: Log.LogMessage -> Log.TimeStamp
getTimeStamp (Log.LogMessage (Log.Error _ ) ts _) = ts

compareTimeStamp :: Log.LogMessage -> Log.LogMessage -> Ordering
compareTimeStamp x y = compare (getTimeStamp x) (getTimeStamp y)

whatWentWrong :: [Log.LogMessage] -> [String]
whatWentWrong lms = map getString (sortBy compareTimeStamp (filter (isErrorAbove 50) lms))