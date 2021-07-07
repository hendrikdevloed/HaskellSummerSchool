{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Exercise4(
    module Exercise2
)where

import Prelude hiding (zip)
import Exercise2 ( HList(..), Append, append )
import Data.Void

-- Zip
-- The usual zip function on (homogeneous) lists is defined as follows:
-- zip :: [a] -> [b] -> [(a,b)]
-- zip [] _ = []
-- zip _ [] = []
-- zip (x:xs) (y:ys) = (x,y) : zip xs ys
-- Exercise 4: Implement a hzip function that zips two heterogeneous lists. How will you handle the
-- situation when the lists have different lengths? What auxiliary type families do you need?

type family Zip as bs where
    Zip '[] '[] = '[]
    Zip (a ': as) (b ': bs) = (a,b) ': Zip as bs
    Zip (a ': as) '[] = (a,()) ': Zip as '[]
    Zip '[] (b ': bs) = ((),b) ': Zip '[] bs

hzip :: HList as -> HList bs -> HList (Zip as bs)
hzip Nil Nil = Nil
hzip Nil (Cons b bs) = Cons ((), b) (hzip Nil bs)
hzip (Cons a as) Nil = Cons (a, ()) (hzip as Nil)
hzip (Cons a as) (Cons b bs) = Cons (a,b) (hzip as bs)
                          
testZip1 :: HList '[(Integer , Bool), (Bool, Integer), ([Char],[Char]), (Double, ())]
testZip1 = hzip (Cons 1 (Cons False (Cons "hello" (Cons 2.5 Nil)))) (Cons True (Cons 5 (Cons "world" Nil)))

