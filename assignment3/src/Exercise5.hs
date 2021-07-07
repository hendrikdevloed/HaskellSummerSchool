{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Exercise5(
    module Exercise4,
    zip
)where

import Exercise4

-- Exercise 5: It would perhaps be better to rule out having lists of different lengths as inputs. To do this,
-- we can define a hzip function with the following type:
hzip :: SameLength as bs -> HList as -> HList bs -> HList (Zip as bs)
-- Define a GADT, SameLength, that enforces two lists of types have the same length. Show that you
-- can use this to require two argument HLists to hzip have the same length.

hzip = undefined

testZip1 :: HList '[(Integer , Bool), (Bool, Integer), ([Char],[Char])]
testZip1 = hzip witness (Cons 1 (Cons False (Cons "hello" (Cons 2.5 Nil)))) (Cons True (Cons 5 (Cons "world" Nil)))
