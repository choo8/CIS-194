module Ex5 where

import Ex3
import Ex4

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = interleave 0
    where interleave n = interleaveStreams (streamRepeat n) (interleave (n + 1))