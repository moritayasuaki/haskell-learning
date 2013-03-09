{- |
Module :   DocExample
Description :   doctest-haddock-example
Copyright :   (c) Morita Yasuaki
License :   BSD liscense
-}

module DocExample where

-- |
-- doubleのテストケースだよ
-- >>> double 4
-- 8

double :: Int -> Int
double a = 2*a

-- ^
-- doubleの他の利用例
-- >>> double 12
-- 24
--
-- double関数の性質
-- prop> double x `div` 2 == x

