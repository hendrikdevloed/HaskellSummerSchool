{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module Exercise2
where

-- Evaluating lambda terms
-- In Haskell, usually all the elements of a list have the same type. Using GADTs, however,we can provide a
-- type-safe interface to lists storing dierent types, also known as heterogeneous lists or hlists for short.
data HList :: [*] -> * where
    Nil :: HList '[]
    Cons :: a -> HList as -> HList (a ': as)

-- Lookup
-- By themselves, these hlists are not very useful. Fortunately, we can define a GADT representing the
-- valid positions in an hlist in the following style:

data Pos :: * -> [*] -> * where
--    Top :: Pos ... ...
--    Pop :: Pos ... ... -> Pos ... ...
-- Exercise 2: Complete the definition of positions above. Intuitively, the Top constructor should refer to
-- the head of the HList; the Pop constructor refers to a position in the tail.


-- Exercise 3: Use this definition to define a type-safe lookup function:
lookup :: Pos b as -> HList as -> b
lookup = undefined