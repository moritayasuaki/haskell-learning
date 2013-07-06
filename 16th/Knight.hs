{-# LANGUAGE FlexibleInstances #-}

module Knight where

import Control.Monad
import Data.Monoid

type KnightPos = ( Int, Int )
type KnightPath = [ KnightPos ]

instance Monad m => Monoid (a -> m a) where
    mempty = return
    mappend = (>=>)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

pmoveKnight :: KnightPath -> [KnightPath]
pmoveKnight (p:ps) = map (\x -> x:p:ps) (moveKnight p)

pathWith hands start end = do
    path <- foldl' (>=>) return (replicate hands pmoveKnight) $ [start]
    guard (head path == end)
    return path

-- |
-- >>> map (\hands -> length $ pathWith hands (6,2) (6,1)) [1..7] 

