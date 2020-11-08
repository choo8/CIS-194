module Ex3 where

import Ex1

data Score = Score Int
    deriving Show

instance Semigroup Score where
    Score s1 <> Score s2 = Score (s1 + s2)

instance Monoid Score where
    mempty = Score 0

score :: Char -> Score
score s
    | s == 'a' || s == 'A' = Score 1
    | s == 'b' || s == 'B' = Score 3
    | s == 'c' || s == 'C' = Score 3
    | s == 'd' || s == 'D' = Score 2
    | s == 'e' || s == 'E' = Score 1
    | s == 'f' || s == 'F' = Score 4
    | s == 'g' || s == 'G' = Score 2
    | s == 'h' || s == 'H' = Score 4
    | s == 'i' || s == 'I' = Score 1
    | s == 'j' || s == 'J' = Score 8
    | s == 'k' || s == 'K' = Score 5
    | s == 'l' || s == 'L' = Score 1
    | s == 'm' || s == 'M' = Score 3
    | s == 'n' || s == 'N' = Score 1
    | s == 'o' || s == 'O' = Score 1
    | s == 'p' || s == 'P' = Score 3
    | s == 'q' || s == 'Q' = Score 10
    | s == 'r' || s == 'R' = Score 1
    | s == 's' || s == 'S' = Score 1
    | s == 't' || s == 'T' = Score 1
    | s == 'u' || s == 'U' = Score 1
    | s == 'v' || s == 'V' = Score 4
    | s == 'w' || s == 'W' = Score 4
    | s == 'x' || s == 'X' = Score 8
    | s == 'y' || s == 'Y' = Score 4
    | s == 'z' || s == 'Z' = Score 10
    | otherwise            = Score 0  

scoreString :: String -> Score
scoreString = mconcat . map score

scoreLine :: String -> JoinList Score String
scoreLine s = (Single (scoreString s) s)