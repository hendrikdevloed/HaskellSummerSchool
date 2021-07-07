{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Exercise0(HList(..))
where

-- In the previous exercise, we saw how to define a type of heterogeneous lists using a GADT:
data HList :: [*] -> * where
    Nil :: HList '[]
    Cons :: a -> HList as -> HList (a ': as)

-- HShow :: [*] -> Constraint
type family HShow as where
    HShow (a ': as) = (Show a, HShow as)
    HShow '[] = ()

showHList :: HShow as => HList as -> String 
showHList Nil = "[]"
showHList (Cons a as) = (show a) ++ ", " ++ (show as)

instance HShow as => Show (HList as) where
    show = showHList

-- Although we can define a lookup function, other familiar list operators are not very easy. Consider the
-- simple (++) operator that appends two lists:
-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
-- Exercise 0: Try naively defining the corresponding function on HLists. Why is this harder?

-- naiveappend :: HList a -> HList b -> HList c
-- one needs to be able to define that c = a concatenated with b on the type level




