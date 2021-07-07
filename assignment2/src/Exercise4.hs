{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Exercise4()
where

import Prelude hiding (lookup)
import Exercise2

-- One application of such heterogeneous lists is in writing interpreters for (typed) languages embedded
-- in Haskell. In this exercise, we will write a small interpreter for the lambda calculus in Haskell. To start,
-- we will define the following data type for lambda terms:
data Term :: [*] -> * -> * where
    App :: Term ctx (s -> t) -> Term ctx s -> Term ctx t
    Lam :: Term (s ': ctx) t -> Term ctx (s -> t)
    Var :: Pos s ctx -> Term ctx s
-- A value of type Term ctx s corresponds to a lambda term of type s with free variables drawn from
-- the list ctx. Note how we use the Pos data type from the previous part to represent variables.
-- Exercise 4: Use the lookup function defined previously to define an interpreter with the following
-- type:
eval :: Term ctx s -> HList ctx -> s
-- Hint: try mapping the Lam constructor to Haskell’s lambda construct and the App constructor to
-- Haskell’s function application. If you’re unsure about how to complete a particular clause, try inserting
-- underscores to help guide the programme’s development.

-- Note: Pos = De Bruyn index of variable instead of variable name

eval (Var idx) vars = lookup idx vars
--eval (Lam (x:ctx) func) = \x -> eval 

testInt = eval program vars
    where
        program = Var (Pop Top)
        vars = Cons False (Cons 5 Nil)

testLam = eval program vars
    where
        -- program = \v1::int -> v1 + 1
        program = App 
        var2 = Var (Pop Top)
        vars = Cons False (Cons 5 Nil)
