module Ex2 where

import Ex1
import Sized

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single m x)
    | n == 0    = Just x
    | otherwise = Nothing
indexJ n (Append m l r)
    | (i <= n) || (n < 0) = Nothing
    | n < li              = indexJ n l
    | otherwise           = indexJ (n - li) r
    where (Size i)  = size m
          (Size li) = size (tag l)

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n (Single m x) = if n > 0 then Empty else (Single m x)
dropJ n (Append m l r)
    | n <= 0    = (Append m l r)
    | n <= li   = (dropJ n l) +++ r
    | otherwise = (dropJ (n - li) r)
    where (Size i)  = size m
          (Size li) = size (tag l)

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n (Single m x) = if n > 0 then (Single m x) else Empty
takeJ n (Append m l r)
    | n >= i    = (Append m l r)
    | n >= li   = l +++ (takeJ (n - li) r)
    | otherwise = (takeJ n l)
    where (Size i)  = size m
          (Size li) = size (tag l)