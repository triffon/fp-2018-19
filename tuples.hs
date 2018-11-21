module Tuples where

import Prelude hiding (fst, snd)

type Point = (Double, Double)
type Triangle = (Point, Point, Point)
type Translation = Point -> Point

p :: Point
p = (1.2, 2.3)

symX :: Translation
symX p = (fst p, -snd p)

t :: Triangle
t = (p, (0, 0), (1.3, 2.8))

fstPoint :: Triangle -> Point
fstPoint (p, _, _) = p

sndPoint :: Triangle -> Point
sndPoint (_, p, _) = p

thdPoint :: Triangle -> Point
thdPoint (_, _, p) = p

applyTranslation :: Translation -> Triangle -> Triangle
applyTranslation trans (p1, p2, p3) = (trans p1, trans p2, trans p3)

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

(f, g) = (sin, cos)
