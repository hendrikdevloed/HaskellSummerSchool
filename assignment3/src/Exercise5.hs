{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Exercise5(
    module Exercise4
)where

import Exercise4

-- Exercise 5: It would perhaps be better to rule out having lists of different lengths as inputs. To do this,
-- we can define a hzip function with the following type:
-- hzip :: SameLength as bs -> HList as -> HList bs -> HList (Zip as bs)
-- Define a GADT, SameLength, that enforces two lists of types have the same length. Show that you
-- can use this to require two argument HLists to hzip have the same length.

-- Solution using type families:
--type family EqLength as bs where
--    EqLength '[] '[] = 'True
--    EqLength '[] '[] = 'True
-- hzipTF :: (EqLength as bs ~ 'True) => HList as -> HList bs -> HList (Zip as bs)


-- Solution using GADT:
data SameLength :: [*] -> [*] -> * where
    BothNil :: SameLength '[] '[]
    BothCons :: SameLength as bs -> SameLength (a ': as) (b ': bs)

hzipGadt :: SameLength as bs -> HList as -> HList bs -> HList (Zip as bs)
hzipGadt BothNil Nil Nil = Nil
hzipGadt (BothCons eqTails) (Cons a as) (Cons b bs) = Cons (a, b) (hzipGadt eqTails as bs)

testZip1 :: HList '[(Integer , Bool), (Bool, Integer), ([Char],[Char])]
testZip1 = hzipGadt witness (Cons 1 (Cons False (Cons "hello" Nil))) (Cons True (Cons 5 (Cons "world" Nil)))
    where
        witness = BothCons (BothCons (BothCons BothNil))

-- Solution using typeclasses
class HZipClass tas tbs where
    hzipClass :: HList tas -> HList tbs -> HList (Zip tas tbs)

instance HZipClass '[] '[] where
    hzipClass Nil Nil = Nil

instance HZipClass tas tbs => HZipClass (ta ': tas) (tb ': tbs) where
    hzipClass (Cons a as) (Cons b bs) = Cons (a,b) (hzipClass as bs)


-- testZip1 :: HList '[(Integer , Bool), (Bool, Integer), ([Char],[Char])]
-- testZip1 = hzip witness (Cons 1 (Cons False (Cons "hello" (Cons 2.5 Nil)))) (Cons True (Cons 5 (Cons "world" Nil)))
