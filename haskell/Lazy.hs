module Lazy where

f = f

g 0 = 5
g x = x + 1

h _ y = y + 1

l []     = 0
l (_:xs) = 1 + l xs

g' 0 = 5
g' _ = 10

