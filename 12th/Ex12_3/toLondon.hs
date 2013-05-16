-- | toLondon.hs from p223-p226 in Learn you haskell great good.

module Main where

import Data.List

data Section = Section { getA :: Int, getB :: Int, getC :: Int }
     deriving (Show)

type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                    , Section 5 90 20
                    , Section 40 2 25
                    , Section 10 8 0
                    ]

data Label = A | B | C deriving (Show)
type Path = [Label]

-- | セクション１つ先までの最短経路
roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, timeA, timeB) (Section a b c) =
    let forwardTimeToA = timeA + a
        crossTimeToA   = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB   = timeA + a + c
        newPathToA = if forwardTimeToA <= crossTimeToA
                        then A:pathA
                        -- 直感的にpathA ++ (A, a)にしたいところだが…
                        -- xs ++ ysは、xsを分解して
                        -- 全部(:)でつなげるということをするから遅い
                        -- P138参照
                        else C:B:pathB
        nextTimeA = min forwardTimeToA crossTimeToA
        newPathToB = if forwardTimeToB <= crossTimeToB
                        then B:pathB
                        else C:A:pathA
        nextTimeB = min forwardTimeToB crossTimeToB
    in (newPathToA, newPathToB,nextTimeA, nextTimeB)

-- | 全セクションの最短経路
optimalPath :: RoadSystem -> (Path, Int)
optimalPath roadSystem =
    let (bestAPath, bestBPath, bestTimeA, bestTimeB) = foldl' roadStep ([], [], 0, 0) roadSystem
    in if bestTimeA <= bestTimeB
            then (reverse bestAPath, bestTimeA)
            else (reverse bestBPath, bestTimeB)

-- | n個ずつリストを分けて、リストのリストを返す
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

-- | main

main :: IO ()
main = do

    contents <- getContents
    let threes = groupsOf 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
--    let roadSystem = heathrowToLondon
        (path,t) = optimalPath roadSystem
        -- concat $ map f xsはconcatMap f xsにまとめられます
        pathString = concatMap show path
        pathTime = t
    putStrLn $ "The best path to take is: " ++ pathString
    putStrLn $ "Time taken: " ++ show pathTime
