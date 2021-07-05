module Lib
where

-- Given the standard type classes for functors, applicative functors and monads:
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
-- class Applicative f => Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b

-- Exercise 1: Give instances for all three classes for the following data types:

data Tree a = Leaf a | Node (Tree a) (Tree a)
data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
data Teletype a =
    Get (Char -> Teletype a)
    | Put Char (Teletype a)
    | Return a

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)
instance Applicative Tree where
    pure = Leaf
    (Leaf f) <*> t = fmap f t
    (Node l r) <*> t = Node (l <*> t) (r <*> t)
instance Monad Tree where
    (Leaf a) >>= f = f a
    (Node l r) >>= f = Node (l >>= f) (r >>= f)

--instance Functor RoseTree where
--    fmap _ RoseLeaf = RoseLeaf
--    fmap f (RoseNode n ts) = RoseNode (f a) (fmap f ts)

instance Functor Teletype where
    fmap f (Return a) = Return (f a)
    fmap f (Put c tt) = Put c (fmap f tt)
    fmap f (Get getch) = Get (\c -> fmap f (getch c))

instance Traversable Tree where
    traverse modify (Leaf a) = fmap pure (modify a) -- pure = Leaf
    traverse modify (Node l r) = 