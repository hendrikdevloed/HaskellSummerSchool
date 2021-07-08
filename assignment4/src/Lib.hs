module Lib(

) where

import GHC.Generics
import Parsec

-- Haskell’s Show and Read classes provide an easy way to display and parse user-defined data structures.
-- Use GHC Generics and some parsing library (uuparsinglib, attoparsec or parsec), define a
-- generic Parse class. You may want to have a look at Generic.Deriving.Show to see how a
-- generic Show instance can be derived.
-- Writing a generic read for all possible constructs is not feasible, but try to cover as much of the language
-- as you can.
-- • Start by handling only “basic” ADTs. To make it more precise, this means that it works for:
--     data Bool = True | False
--     data IntTree = Leaf Int | Node IntTree IntTree
-- • Then take fixity of operators is taken into account. To make it more precise, that means that the
--   parser can handle Leaf 1 :|: (Leaf 2 :|: Leaf 3) when IntTree is declared as:
--     data IntTree = Leaf Int | IntTree :|: IntTree
-- • Finally, support record labels. To make it more precise, that means that the parser can handle
--   Number { n = 1 } for a data type declared as:
data Number = Number { n :: Int }

