module Lists where

import Prelude
  hiding (head, tail, null, length,
          enumFromTo, enumFromThenTo, (++), reverse)

x :: Int
x = 2

head :: [a] -> a
head (h:_) = h

tail :: [a] -> [a]
tail (_:t) = t

null :: [a] -> Bool
null [] = True
null _  = False

length :: [a] -> Int
length [] = 0
length l  = 1 + length (tail l)

enumFromTo :: Int -> Int -> [Int]
enumFromTo from to
 | from > to     = []
 | otherwise     = from:enumFromTo (from+1) to

enumFromThenTo :: Int -> Int -> Int -> [Int]
enumFromThenTo from then' to
 | from > to     = []
 | otherwise     = from:enumFromThenTo then' (then' + δ) to
   where δ = then' - from

(++) :: [a] -> [a] -> [a]
[] ++ b  = b
a  ++ b  = head a : tail a ++ b

reverse :: [a] -> [a]
reverse [] = []
reverse l  = reverse (tail l) ++ [head l]
