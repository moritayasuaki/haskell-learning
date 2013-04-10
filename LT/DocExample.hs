{- |
Module :   DocExample
Description :   doctest-haddock-example
Copyright :   Morita Yasuaki
License :   BSD liscense
-}

module DocExample where

-- |
-- Unit Test for `double`
--
-- >>> double 4
-- 8

double :: Int -> Int
double a = 2 * a

-- |
-- >>> double 5.0
-- 10

-- ^
-- Other Usage of `double`
--
-- >>> 1 `div` 0
-- *** Exception: divide by zero
--
-- Property of `double`
--
-- prop> double x `div` 2 == x

