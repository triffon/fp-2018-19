module FunctorsMonads where

import Main (getInt)

import GHC.Base (returnIO)

import Data (BinTree(..), Tree(..), TreeList(..), mapTree, foldrTree, Maybe(..), Either(..))

import Prelude hiding (Functor(..), (<$>), Maybe(..), Either(..),
                      Applicative(..), sequenceA, return, (>>=), Monad(..))

class Countable c where
  count :: c a -> Integer

instance Countable [] where
  -- count :: [a] -> Integer
  count = fromIntegral . length

instance Countable BinTree where
  count Empty = 0
  count (Node _ lt rt) = 1 + count lt + count rt

instance Countable Tree where
  count (Tree _ st) = 1 + count st

instance Countable TreeList where
  count None = 0
  count (SubTree t ts) = count t + count ts

class Listable c where
  elements :: c a -> [a]

instance Listable [] where
  -- elements :: [a] -> [a]
  elements = id

instance Listable BinTree where
--  elements Empty = []
--  elements (Node x lt rt) = x : elements lt ++ elements rt
  elements = Data.foldrTree (:) []

instance Listable Tree where
  elements (Tree x tl) = x : elements tl

instance Listable TreeList where
  elements None = []
  elements (SubTree t ts) = elements t ++ elements ts

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap = map

instance Functor BinTree where
  fmap = mapTree

instance Functor Tree where
  fmap f (Tree x tl) = Tree (f x) (fmap f tl)

instance Functor TreeList where
  fmap _ None = None
  fmap f (SubTree t ts) = SubTree (fmap f t) (fmap f ts)

(<$>) :: (Functor f) => (a -> b) -> f a -> f b
(<$>) = fmap

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)
--  fmap _ _ = Nothing

x :: Either Int String
x = Left 5

y :: Either Int String
y = Right "hello"

instance Functor (Either r) where
  -- fmap :: (a -> b) -> Either r a -> Either r b
  fmap _ (Left x) = Left x
  fmap f (Right y) = Right (f y)

instance Functor ((,) r) where
  -- fmap :: (a -> b) -> (r, a) -> (r, b)
  fmap f (x, y) = (x, f y)

instance Functor ((->) r) where
  -- fmap :: (a -> b) -> (r -> a) -> (r -> b)
  fmap = (.)

instance Functor IO where
  -- fmap :: (a -> b) -> IO a -> IO b
  fmap f ia = do
                x <- ia
                returnIO $ f x

class (Functor f) => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
--  fmap f x = pure f <*> x

instance Applicative Maybe where
  pure = Just
  Nothing  <*> _        = Nothing
  _        <*> Nothing  = Nothing
  (Just f) <*> (Just x) = Just (f x)

-- !!!
-- instance Applicative ((,) r) where
  -- pure :: a -> (r, a)
  -- pure x = (?, x)

instance Applicative (Either r) where
  -- pure :: a -> Either r a
  pure = Right
  -- (<*>) :: Either r (a -> b) -> Either r a -> Either r b
  Left x  <*> _       = Left x
  _       <*> Left x  = Left x
  Right f <*> Right y = Right (f y)

instance Applicative [] where
  -- pure :: a -> [a]
--  pure x = [x]
  pure = (:[])
  fs <*> xs = [ f x | f <- fs, x <- xs ]

data ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Ord, Show, Read)

instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  -- fmap f zl = ZipList (map f (getZipList zl))
  fmap f = ZipList . map f . getZipList

instance Applicative ZipList where
  -- pure x = ZipList [x]
  pure = ZipList . (:[])
  ZipList fs <*> ZipList xs = ZipList $ zipWith ($) fs xs

instance Applicative ((->) r) where
  -- pure :: a -> (r -> a)
  -- pure x y = x
  pure = const
  -- (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
  f <*> g = \x -> (f x) (g x)

instance Applicative IO where
  -- pure :: a -> IO a
  pure = returnIO
  fio <*> xio = do
                    f <- fio
                    x <- xio
                    returnIO $ f x
  
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

sequenceA :: (Applicative f) => [f a] -> f [a]
-- sequenceA []     = pure []
-- sequenceA (x:xs) = liftA2 (:) x (sequenceA xs)
sequenceA = foldr (liftA2 (:)) (pure [])

class (Applicative m) => Monad m where
  return :: a -> m a
  return = pure

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  x >> y = x >>= (\_ -> y)

  fail :: String -> m a
  fail = error

instance Monad Maybe where
  Nothing >>=  _ = Nothing
  (Just x) >>= f = f x

getAt' :: [a] -> Int -> Maybe a
getAt' []     _ = Nothing
getAt' (x:_ ) 0 = Just x
getAt' (_:xs) n = getAt' xs (n - 1)

instance Monad [] where
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  -- ? :: [[b]] -> [b]
  -- xs >>= f = concat $ map f xs
  (>>=) = flip concatMap

instance Monad ((->) r) where
  -- (>>=) :: (r -> a) -> (a -> (r -> b)) -> (r -> b)
  f >>= g = \x -> g (f x) x
