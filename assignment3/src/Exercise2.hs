{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Exercise2(
    module Exercise1,
    append
)where

import Exercise1

-- Exercise 2: Use the Append type family to define a version of (++) that works for heterogeneous
-- lists.
append :: HList as -> HList bs -> HList (Append as bs)
append Nil bs = bs
append (Cons a as) bs = Cons a (append as bs)

testAppend1 = append (Cons 1 (Cons False (Cons "hello" Nil))) (Cons True (Cons 5 (Cons "world" Nil)))
