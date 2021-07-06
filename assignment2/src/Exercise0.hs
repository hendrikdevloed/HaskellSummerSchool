{-# LANGUAGE GADTs #-}
module Exercise0
where

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

-- A good way to understand GADTs is to complete the definition of the eval function step-by-step. Add
-- a new case for the LitI constructor, leaving a hole for the right-hand side:

eval :: Expr a -> a
--eval (LitI i) = _
--eval e = undefined

-- Exercise 0: What is the type of the hole? What values are in scope?
-- Gradually complete the definition of eval.

--eval (LitI i) = _
-- * Found hole: _ :: Int
--   * In the expression: _

eval (LitI i) = i
eval (LitB b) = b
eval (IsZero ei) = eval ei == 0
eval (Plus ei1 ei2) = eval ei1 + eval ei2
eval (If econd ethen eelse) = if eval econd then eval ethen else eval eelse
