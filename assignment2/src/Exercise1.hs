{-# LANGUAGE GADTs #-}
module Exercise1
where

import Data.Either

-- Exercise 1: Can you extend the expression language with new operators, such as comparisons (<)
-- and (==)? What about adding pairs and projections (fst and snd)? Or the Either type? What new
-- constructors do you need? How can you extend the evaluator accordingly?

--type ComparisonExpr a = Expr a -> Expr a -> Expr Bool

data Expr a where
    -- integer literals
    LitI :: Int -> Expr Int
    -- boolean literals
    LitB :: Bool -> Expr Bool
    -- check if a number is 0 or not
    IsZero :: Expr Int -> Expr Bool
    -- integer addition
    Plus :: Expr Int -> Expr Int -> Expr Int
    -- conditionals/if-then-else
    If :: Expr Bool -> Expr a -> Expr a -> Expr a
    -- Comparison
    Gt :: Ord a => Expr a -> Expr a -> Expr Bool
    Geq :: Ord a => Expr a -> Expr a -> Expr Bool
    Lt :: Ord a => Expr a -> Expr a -> Expr Bool
    Leq :: Ord a => Expr a -> Expr a -> Expr Bool
    Eq :: Eq a => Expr a -> Expr a -> Expr Bool
    -- Pairs and projections
    Pair :: Expr a -> Expr b -> Expr (a,b)
    Fst :: Expr (a,b) ->  Expr a
    Snd :: Expr (a,b) ->  Expr b
    --
    Lft :: Expr a -> Expr (Either a b)
    Rgt :: Expr b -> Expr (Either a b)
    Eith :: (a -> c) -> (b -> c) -> Expr (Either a b) -> Expr c


eval :: Expr a -> a
eval (LitI i) = i
eval (LitB b) = b
eval (IsZero ei) = eval ei == 0
eval (Plus ei1 ei2) = eval ei1 + eval ei2
eval (If econd ethen eelse) = if eval econd then eval ethen else eval eelse
eval (Gt e1 e2) = evalComparisonUsing (>) e1 e2
eval (Lt e1 e2) = evalComparisonUsing (<) e1 e2
eval (Geq e1 e2) = evalComparisonUsing (>=) e1 e2
eval (Leq e1 e2) = evalComparisonUsing (<=) e1 e2
eval (Eq e1 e2) = evalComparisonUsing (==) e1 e2
eval (Pair ea eb) = (eval ea, eval eb)
eval (Fst eab) = fst $ eval eab
eval (Snd eab) = snd $ eval eab
eval (Lft ea) = Left (eval ea)
eval (Rgt eb) = Right (eval eb)
eval (Eith fac fbc eab) = either fac fbc (eval eab)

evalComparisonUsing :: (a -> a -> Bool) -> Expr a -> Expr a -> Bool
evalComparisonUsing op e1 e2 = eval e1 `op` eval e2
