{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Exercise1(
    module Exercise0,
    Append(..)
)where

import Exercise0

-- Type-level append
-- We would like to define an append function with the following type:
-- append :: HList as -> HList bs -> HList (as ++ bs)
-- Unfortunately, there is no type (++) to append two lists, but we can implement one ourselves.
-- Exercise 1: Define a type family:
type family Append as bs where
--- ....
-- You will need to provide two clauses: one for the empty list; and one for the case a ': as.
    Append '[] bs = bs
    Append (a ': as) bs = a ': Append as bs
