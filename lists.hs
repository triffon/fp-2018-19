module Lists where

import Prelude
  hiding (head, tail, null, length,
          enumFromTo, enumFromThenTo, (++), reverse,
          foldr, foldl, scanr, scanl, zip, zipWith, unzip,
          takeWhile, dropWhile)

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

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _  nv []      = nv
foldr op nv (x:xs)  = x `op` r
  where r = foldr op nv xs

-- head (scanr op nv xs) == foldr op nv xs
-- last (scanr op nv xs) == nv

{-
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _  nv []     = [nv]
scanr op nv (x:xs) = x `op` r:rs
  where rs@(r:_) = scanr op nv xs
-}

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr op nv = foldr (\x rs@(r:_) -> x `op` r:rs) [nv]

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _  nv []      = nv
foldl op nv (x:xs)  = foldl op (nv `op` x) xs

-- last (scanl op nv xs) == foldl op nv xs
-- head (scanl op nv xs) == nv

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _  nv []      = [nv]
scanl op nv (x:xs)  = nv : scanl op (nv `op` x) xs
-- scanl op nv = foldr (\x r -> [nv] ???

zip :: [a] -> [b] -> [(a,b)]
{-
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y):zip xs ys
-}

unzip :: [(a,b)] -> ([a],[b])
unzip = foldr (\(x,y) (xs,ys) -> (x:xs,y:ys)) ([],[])

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith op []     _      = []
zipWith op _      []     = []
zipWith op (x:xs) (y:ys) = x `op` y:zipWith op xs ys

zip = zipWith (,)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x r -> if p x then x:r else []) []
  -- r == takeWhile p (tail l)
--dropWhile :: (a -> Bool) -> [a] -> [a]
--dropWhile p = foldr (\x r -> if p x then r else x:?) []

dropWhile _ []     = []
dropWhile p r@(x:xs)
  | p x       = dropWhile p xs
  | otherwise = r
