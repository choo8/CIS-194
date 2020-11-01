module Ex2 where

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)
    
insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = (Node 0 Leaf x Leaf)
insertNode x (Node h t1 k t2)
    | balance <= 0 = updateHeight (Node h (insertNode x t1) k t2)
    | otherwise    = updateHeight (Node h t1 k (insertNode x t2))
    where balance = getBalance (Node h t1 k t2)
    
getHeight :: Tree a -> Integer
getHeight Leaf = -1
getHeight (Node h _ _ _) = h

getLeft :: Tree a -> Tree a
getLeft Leaf = Leaf
getLeft (Node _ t1 _ _) = t1

getRight :: Tree a -> Tree a
getRight Leaf = Leaf
getRight (Node _ _ _ t2) = t2
    
updateHeight :: Tree a -> Tree a
updateHeight Leaf = Leaf
updateHeight (Node _ t1 k t2) = (Node (1 + (max (getHeight t1) (getHeight t2))) (updateHeight t1) k (updateHeight t2))

getBalance :: Tree a -> Integer
getBalance Leaf = 0
getBalance (Node _ t1 _ t2) = (getHeight t1) - (getHeight t2)
    
foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf
