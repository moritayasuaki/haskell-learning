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

-- ^
-- Other Usage of `double`
--
-- >>> double 12
-- 24
--
-- Property of `double`
--
-- prop> double x `div` 2 == x

