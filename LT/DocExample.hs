-- | 
-- This module is an example of how to use doctest and haddock.
-- Unfortunately, haddock don't supoprt UTF-8 and `prop>` notation.

module DocExample where

-- |
-- Test cases of `double`
--
-- >>> double 2
-- 4
-- >>> double 4
-- 8
-- 
-- `double`'s properties :
--
-- @
--   prop> double x `div` 2 == x
--   prop> even x ==> double (x `div` 2) == x
--   prop> odd x  ==> double (x `div` 2) == x - 1
-- @

double :: Int -> Int
double a = 2 * a

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

-- |
-- >>> children Empty
-- []
-- >>> children (Node 1 (Node 2 Empty Empty) (Node 3 Empty Empty))
-- [Node 2 Empty Empty,Node 3 Empty Empty]

children Empty = []
children (Node a left right) = [left, right]
