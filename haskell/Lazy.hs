module Lazy where

import Prelude hiding (enumFrom, repeat, flip)

f = f

g 0 = 5
g x = x + 1

h _ y = y + 1

l []     = 0
l (_:xs) = 1 + l xs

g' 0 = 5
g' _ = 10

ifThenElse True  x _ = x
ifThenElse False _ y = y

ones = 1 : ones

enumFrom x = x : enumFrom (x + 1)

repeat x = [x,x..]

pairs = concat [ [(x, y), (y, x), (x, x)] | x <- [0..], y <- [0..x-1] ]

pythagoreanTriples = [ (a, b, c) | c <- [0..], b <- [0..c-1], a <- [0..b-1],
                                 a^2 + b^2 == c^2, gcd a b == 1 ]

triplets = iterate (map (+3)) [3,2,1]

flip f x y = f y x

-- switch x = seq x $ switch (1 - x)
switch x = switch $! (1 - x)

