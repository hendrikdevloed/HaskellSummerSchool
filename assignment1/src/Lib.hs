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

instance Functor RoseTree where
    fmap _ RoseLeaf = RoseLeaf
    fmap f (RoseNode n ts) = RoseNode (f n) (fmap (fmap f) ts)
instance Applicative RoseTree where
    pure a = RoseNode a []
    RoseLeaf <*> _ = RoseLeaf
    _ <*> RoseLeaf = RoseLeaf
    _ <*> _ = undefined

instance Functor Teletype where
    fmap f (Return a) = Return (f a)
    fmap f (Put c tt) = Put c (fmap f tt)
    fmap f (Get getch) = Get (fmap f . getch)

instance Foldable Tree where
    -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap f (Leaf a) = f a
    foldMap f (Node l r) = foldMap f l <> foldMap f r

instance Traversable Tree where
    traverse modify (Leaf a) = fmap pure (modify a) -- pure = Leaf
    traverse modify (Node l r) = undefined

teletypeexample1 = Get (\x -> if x == 'n' then Return 1 else Put 'c' (Return 0))
