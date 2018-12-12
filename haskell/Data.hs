module Data where

import Data.Function hiding (on)

class Measurable a where
  size :: a -> Int
  empty :: a -> Bool
  empty = (==0) . size

on f g x y = f (g x) (g y)
-- liftM2 f g h x = f (g x) (h x)

larger :: (Measurable a) => a -> a -> Bool
--larger x y = size x > size y
larger  = (>) `on` size

instance Measurable Int where
  size 0 = 0
  size n = 1 + size (n `div` 10)

instance (Measurable a, Measurable b) => Measurable (a,b) where
  size (x, y) = size x + size y

x::Int
x = 100

instance (Measurable a) => Measurable [a] where
  size = sum . map size
