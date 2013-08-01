module Prob where

import Data.Ratio
import Control.Monad
import Control.Applicative
import Data.List(all,groupBy)

newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
    f `fmap` (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap multAll xs
    where multAll (Prob innerxs, p) = map (\(x, r) -> (x, p*r)) innerxs

instance Monad Prob where
    return x = Prob [(x, 1 % 1)]
    m >>= f = flatten . fmap f $ m
    fail _ = Prob []


data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2),(Tails,1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]


-- |
-- >>> getProb flipThree
-- [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]

flipThree :: Prob Bool
flipThree = do
    a <- coin
    b <- coin
    c <- loadedCoin
    return $ all (==Tails) [a,b,c]


-- |
-- 等価な事象に関して確率の和を取り、shrinkする
-- >>> getProb $ shrink flipThree
-- [(False,31 % 40),(True,9 % 40)]

shrink :: Eq a => Prob a -> Prob a
shrink (Prob ps) =
    Prob $ map (foldl1 (%+%)) (groupBy' (%==%) ps)
    where (a,pa) %==% (b,pb) = a == b
          (a,pa) %+% (b,pb) | (a == b) = (a,pa + pb)

-- |
-- >>> groupBy' (==) [1,1,1,0,0,1,1,1]
-- [[1,1,1,1,1,1],[0,0]]

groupBy' :: (a -> a -> Bool) ->  [a] -> [[a]]
groupBy' eq (x:xs) =
    (x:together) : groupBy' eq other
    where together = filter (eq x) xs
          other = filter (not . eq x ) xs
groupBy' _ _ = []
