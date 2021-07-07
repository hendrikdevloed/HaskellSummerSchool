{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Exercise2
    ( Pos(..)
    , HList(..)
    , lookup
    ) where

import Prelude hiding (lookup)

-- Evaluating lambda terms
-- In Haskell, usually all the elements of a list have the same type. Using GADTs, however,we can provide a
-- type-safe interface to lists storing different types, also known as heterogeneous lists or hlists for short.
data HList :: [*] -> * where
    Nil :: HList '[]
    Cons :: a -> HList as -> HList (a ': as)

-- Lookup
-- By themselves, these hlists are not very useful. Fortunately, we can define a GADT representing the
-- valid positions in an hlist in the following style:


type Pos :: * -> [*] -> *
data Pos x xs where


--data Pos :: * -> [*] -> * where
--    Top :: Pos ... ...
--    Pop :: Pos ... ... -> Pos ... ...
-- Exercise 2: Complete the definition of positions above. Intuitively, the Top constructor should refer to
-- the head of the HList; the Pop constructor refers to a position in the tail.
-- '[Int, Char, Bool]
-- Top :: Pos Int '[Int, Char, Bool]
-- Pop Top :: Pos Char '[Int, Char, Bool]
-- Pop (Pop Top) :: Pos Bool '[Int, Char, Bool]
    Top :: Pos a (a ': rest)
    Pop :: Pos a rest -> Pos a (b ': rest)

-- Exercise 3: Use this definition to define a type-safe lookup function:
lookup :: Pos b as -> HList as -> b
lookup Top (Cons a rest) = a
lookup (Pop more) (Cons _ rest) = lookup more rest
--
x = lookup (Pop Top)  (Cons 1 (Cons 'a' (Cons True Nil)))
