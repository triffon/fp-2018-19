module Data where

import Data.Function hiding (on)

import Prelude hiding (Maybe, Nothing, Just, Either, Left, Right)

type UnaryFunction a = a -> a

type Dictionary k v = [(k,v)]

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

data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun
             deriving (Eq, Ord, Show, Read, Enum)
today = Wed

haveLectures Wed = True
haveLectures _   = False

type Name = String
type Score = Int

s :: String
s = "Peter"
n :: Name
n = s

{-
data Player = Player Name Score
katniss = Player "Katniss Everdeen" 50
getName (Player name _) = name
-}

data Player = Player { name :: Name, score :: Score }
              deriving (Eq, Ord, Show, Read)
-- katniss = Player "Katniss Everdeen" 50
katniss = Player { score = 50, name = "Katniss Everdeen" }


{-
type Player = (Name, Score)
katniss :: Player
katniss = ("Katniss Everdeen", 50)
getName :: Player -> String
getName (name,_) = name

type Student = (Name, FN)
type FN = Int
kat :: Student
kat = katniss
-}

data Shape = Circle     { radius :: Double }        |
             Rectangle  { width, height :: Double }
                deriving (Eq, Ord, Show, Read)

circle = Circle 5
rect   = Rectangle 3 4

-- data Strange = Player { ... } | Student { ... }

area :: Shape -> Double
area (Circle r)      = pi * r^2
area (Rectangle w h) = w * h


{-
class Shape a where
  area :: a -> Double

data Circle    = Circle    { radius :: Double }
data Rectangle = Rectangle { width, height :: Double }

instance Shape Circle where
  area (Circle r)      = pi * r^2

instance Shape Rectangle where
  area (Rectangle w h) = w * h

circle = Circle 5
rect   = Rectangle 3 4
-}

inscribe :: Shape -> Shape
inscribe (Circle r)      = Rectangle (2*r) (2*r)
inscribe (Rectangle w h) = Circle (sqrt (w^2 + h^2))

{-
instance Eq Shape where
  Circle r1       == Circle r2       = r1 == r2
  Rectangle w1 h1 == Rectangle w2 h2 = w1 == w2 && h1 == h2
  _               == _               = False

instance Show Shape where
  show (Circle r)      = "Circle " ++ show r
  show (Rectangle w h) = "Rectangle " ++ show w ++ " " ++ show h
-}

data Maybe a = Nothing | Just a
             deriving (Eq, Show, Ord, Read)

getAt :: [a] -> Int -> Maybe a
getAt []     _ = Nothing
getAt (x:_)  0 = Just x
getAt (_:xs) n = getAt xs (n - 1)

data Either a b = Left a | Right b
                deriving (Eq, Show, Read, Ord)

type CharOrInt = Either Char Int
c :: Char
c = 'c'
i :: Int
i = 5

ec :: CharOrInt
ec = Left c
ei :: CharOrInt
ei = Right i

searchBest :: [Player] -> Either Score [Player]
searchBest players
  | length bestPlayers == 1    = Left bestScore
  | otherwise                  = Right bestPlayers
   where bestScore   = maximum $ map score players
         bestPlayers = filter ((bestScore ==) . score) players
mario = Player { name = "Mario", score = 45 }

-- template <typename K, typename V> class Dictionary ....
-- template <typename V> using DictionaryInt = Dictionary<Int, V>

{-
union ScoreOrPlayer {
  int score;
  List<Player> players;
};

template <typename A, typename B>
union Either {
  A left;
  B right;
};
-}

{-
struct Node {
  int data;
  Node* next;
};
-}

data Nat = Zero | Succ Nat
         deriving (Eq, Show, Read, Ord)

five = Succ $ Succ $ Succ $ Succ $ Succ $ Zero

toNat :: Integer -> Nat
toNat 0 = Zero
toNat n = Succ $ toNat $ n - 1

fromNat :: Nat -> Integer
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n

plus :: Nat -> Nat -> Nat
plus Zero     n = n
plus (Succ m) n = Succ (plus m n)

data Bin = One | BitZero Bin | BitOne Bin
         deriving (Eq, Ord, Show, Read)

-- 6₁₀ = 110₂
-- six = BitZero $ BitOne One
(%) = flip ($)
six = One % BitOne % BitZero

fromBin :: Bin -> Integer
fromBin One         = 1
fromBin (BitZero b) = 2 * fromBin b + 0
fromBin (BitOne  b) = 2 * fromBin b + 1

toBin :: Integer -> Bin
toBin 1 = One
toBin n
  | even n    = BitZero n2
  | otherwise = BitOne  n2
  where n2 = toBin $ n `div` 2

succBin :: Bin -> Bin
succBin One         = One % BitZero
succBin (BitZero b) = BitOne b
succBin (BitOne  b) = BitZero (succBin b)
-- ....1 + 1 = ? 

data Crazy = A Crazy
           deriving (Eq, Ord, Show, Read)
crazy = A crazy

revealCrazy (A c) = c

data List a = Nil | Cons { listHead :: a, listTail :: List a }
            deriving (Eq, Ord, Show, Read)
-- data Nat = Zero | Succ Nat
-- data Unit = Unit
-- List Unit <-> Nat, Cons Unit <-> Succ, Zero <-> Nil
l = Cons 1 $ Cons 2 $ Cons 3 $ Nil

lengthList :: List a -> Integer
lengthList Nil        = 0
lengthList (Cons _ t) = 1 + lengthList t

fromList :: List a -> [a]
fromList Nil        = []
fromList (Cons h t) = h : fromList t

toList :: [a] -> List a
toList []      = Nil
toList (x:xs)  = Cons x $ toList xs

Nil       +++ l = l
Cons x xs +++ l = Cons x $ xs +++ l

data BinTree a = Empty | Node { root  :: a,
                                left  :: BinTree a,
                                right :: BinTree a }
               deriving (Eq, Ord, Show, Read)

leaf x = Node x Empty Empty

t = Node 3 (leaf 1) (leaf 5)

depth :: BinTree a -> Integer
depth Empty        = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

leaves :: BinTree a -> [a]
leaves Empty                = []
leaves (Node x Empty Empty) = [x]
leaves (Node _ l     r    ) = leaves l ++ leaves r

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree _ Empty        = Empty
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

foldrTree :: (a -> b -> b) -> b -> BinTree a -> b
foldrTree _  nv Empty        = nv
foldrTree op nv (Node x l r) = foldrTree op (op x (foldrTree op nv r)) l
--foldrTree op nv (Node x l r) = op x (foldrTree op (foldrTree op nv r) l)

-- data Tree a     = Tree { root :: a, subtrees :: [a] }
data Tree a     = Tree { val :: a, subtrees :: TreeList a }
                deriving (Eq, Ord, Show, Read)
data TreeList a = None | SubTree { firstTree :: Tree a,
                                   restTrees :: TreeList a }
                deriving (Eq, Ord, Show, Read)

leafTree x = Tree x None
tree = Tree 1 $ SubTree (leafTree 2)
              $ SubTree (Tree 3 $ SubTree (leafTree 4) $ None)
              $ SubTree (leafTree 5) $ None

depthTree :: Tree a -> Integer
depthTree (Tree _ ts) = maxDepthTrees ts + 1

maxDepthTrees :: TreeList a -> Integer
maxDepthTrees None           = 0
maxDepthTrees (SubTree t ts) = max (depthTree t) (maxDepthTrees ts)

level :: Integer -> Tree a -> [a]
level 0 (Tree x _ ) = [x]
level n (Tree _ ts) = levelTrees (n-1) ts

levelTrees :: Integer -> TreeList a -> [a]
levelTrees _ None           = []
levelTrees n (SubTree t ts) = level n t ++ levelTrees n ts
