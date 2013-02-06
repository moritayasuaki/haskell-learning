-- 2 arguments functions list -> number -> 1 arguments function list

module Test where

import Data.List

type ListZipper a = ([a],[a])

ops :: [Int -> Int -> Int]
ops = [(*),(+),(-)]

digits :: [Int]
digits = [1..9]


komachi :: ListZipper Int -> ListZipper Int
komachi (xs, [b]) = [x' `op` b| op <- ops, x' <- (komachi xs)]
komachi ([x], bs) = [x `op` b'| op <- ops, b' <- (komachi bs)]
komachi (x:xs, bs) = [x' `op` b'| op <- ops, x' <- (komachi xs), b' <- (komachi bs) ]

-- | 
-- >>> take 1 [ns| ns <- permutations digits, n <- komachi ([1],[2..9]), n == 2013]
