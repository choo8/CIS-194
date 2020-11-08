{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Ex4 where

import Buffer
import Editor
import Sized
import Ex1
import Ex2
import Ex3

size2 :: JoinList (Score, Size) String -> Size
size2 Empty = Size 0
size2 (Single _ _) = Size 1
size2 (Append (_, m) _ _) = m

scoreLine2 :: String -> JoinList (Score, Size) String
scoreLine2 s = (Single (scoreString s, Size 1) s)

instance Buffer (JoinList (Score, Size) String) where
    toString Empty = ""
    toString (Single _ x) = x
    toString (Append _ l r) = toString l ++ "\n" ++ toString r

    fromString s = foldl1 (+++) (map scoreLine2 (lines s))

    line = indexJ

    replaceLine n ln Empty = Empty
    replaceLine n ln (Single m x)
        | n == 0 = (Single (scoreString ln, Size 1) ln)
        | otherwise = (Single m x)
    replaceLine n ln (Append (Score s, Size i) l r)
        | n < 0 || n >= i = (Append (Score s, Size i) l r)
        | n < sl          = (replaceLine n ln l) +++ r
        | n >= sl         = l +++ (replaceLine (n - sl) ln r)
        where sl = getSize (size2 l)

    numLines Empty = 0
    numLines (Single (_, Size n) _) = n
    numLines (Append (_, Size n) _ _) = n 

    value Empty = 0
    value (Single (Score n, _) _) = n
    value (Append (Score n, _) _ _) = n

main = runEditor editor $ scoreLine2 "This buffer is for notes you don't want to save, and for" +++ scoreLine2 "evaluation of steam valve coefficients." +++ scoreLine2 "To load a different file, type the character L followed" +++ scoreLine2 "by the name of the file."