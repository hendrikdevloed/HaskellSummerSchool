module SmoothPermsSlow
    ( perms
    , smoothPerms
    ) where

-- In this assignment we want to build a library to generate smooth permutations. Given a list of integers
-- xs and an integer d, a smooth permutation of xs with maximum distance d is a permutation in which
-- the difference of any two consecutive elements is at less than d.
-- A naïve implementation just generates all the permutations of a list,

split [] = []
split (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- split xs]

-- |Return the permutations of a list
perms [] = [[]]
perms xs = [(v:p) | (v, vs) <- split xs, p <- perms vs]

-- and then filters out those which are smooth,

smooth n (x:y:ys) = abs (y - x) < n && smooth n (y:ys)
smooth _ _ = True

-- |Return the permutations of a list that are smooth
smoothPerms :: Int -> [Int] -> [[Int]]
smoothPerms n xs = filter (smooth n) (perms xs)
