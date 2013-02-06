{-|

  Haskell-Learning #4 

  Exercises
    Jan, 9, 2013   Morita Yasuaki

-}

{-# Options -Wall #-}

module Exercise where

import Data.List

-- |
--
-- 'placeOf' return a place number of digits
--
-- >>> placeOf 2 4321
-- 2
-- >>> placeOf 3 4321
-- 3
-- >>> placeOf 4 4321
-- 4


placeOf :: Int -> Int -> Int
placeOf r _ | r <= 0 = error "first augument of placeOf must be positive"
placeOf _ d | d < 0  = error "second augument of placeOf must be non-negative"
placeOf 1 d = d `mod` 10
placeOf r d = placeOf (r-1) $ d `div` 10

-- ^
--
-- >>> length [n|n <- [100..999], placeOf 3 n == placeOf 1 n]
-- 90


-- |
-- >>> fib 0
-- 0
-- >>> fib 1
-- 1
-- >>> fib 20
-- 6765
-- >>> fib 100
-- 354224848179261915075

fib :: Integer -> Integer
fib n = fibs 0 1 !! (fromInteger n::Int)
  where
    fibs a b = a : fibs b (a+b)


-- |
-- >>> mySum [1,2,3,4]
-- 10

mySum :: [Int] -> Int
mySum = foldl' (+) 0


-- |
-- >>> oddEven [1,4,5,6,7,10,11]
-- ([1,5,7,11],[4,6,10])

oddEven :: [Int] -> ([Int], [Int])
oddEven = partition odd



-- |
-- I think this definition is more natural for Run Length 'uncompress'.
-- 
-- Test of `uncompress'`
--
-- >>> uncompress' [(3,1),(1,0),(1,1),(4,0),(1,1)]
-- [1,1,1,0,1,0,0,0,0,1]

uncompress' :: [(Int, a)] -> [a]
uncompress' = concatMap $ uncurry replicate

-- |
-- Definition of 'uncompress' on the given type
-- 
-- Test of `uncompress` 
--
-- >>> uncompress [[3,1],[1,0],[1,1],[4,0],[1,1]]
-- [1,1,1,0,1,0,0,0,0,1]

uncompress :: [[Int]] -> [Int]
uncompress = uncompress' . map pairify
  where pairify (x:y:[]) = (x,y)
        pairify _ = error "invalid Run Length data was applied `uncompress`"


-- | 
-- 'compress' is inverse of 'uncompress'
--
-- >>> compress [1,1,1,0,1,0,0,0,0,1]
-- [(3,1),(1,0),(1,1),(4,0),(1,1)]


compress :: Eq a => [a] -> [(Int,a)]
compress [] = []
compress ls@(x:_) = (length matched, x) : compress rest
  where (matched, rest) = span (x==) ls


-- | 
-- Another definition of 'compress'
--
-- @
-- prop> compress x == compress' x 
--          where types = x :: [Int]
-- @

compress' :: Eq a => [a] -> [(Int,a)]
compress' = map (\x -> (length x, head x)) . group

-- ^
-- Property of compress and uncompress'
--
-- > uncompress' . compress == id
--
-- @
-- prop> (uncompress' . compress $ x) == x
--          where types = x :: [Int]
-- 
-- prop> (uncompress' . compress $ x) == x 
--          where types = x :: [Bool]
-- @



-- |
-- Tests of log2
--
-- >>> map log2 [1..15]
-- [0,1,1,2,2,2,2,3,3,3,3,3,3,3,3]

log' :: Integral a => a -> a -> a
log' b _ | b == 1 = error "first argument of log' must not be equal 1"
         | b <= 0 = error "first argument of log' must be positive"

log' b n | n == 1    = 0
         | n > 1     = succ $ log' b (n `div` b)
         | otherwise = error "second argument of log' must be positive"

log2 :: Int -> Int
log2 = log' 2

-- ^
-- Property of log and (^)
-- 
-- note : Overflow is occured in (2 ^ n) for (n >= 31) 
--
-- @
-- 
-- prop> (0 < n && n < 31) ==> n == log2 (2 ^ n)
--
-- @ 
--
-- prop> (m > 0 && b > 1) ==> m == log' b (b ^ m) 
--         where types = m :: Integer
-- @

