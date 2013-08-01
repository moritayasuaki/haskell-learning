{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Knight where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.List
import Control.Monad.Trans.Writer
import Data.Monoid

type Pos = ( Int, Int )
type Path = (WriterT [Pos] [])

moveKnight :: Pos -> [Pos]
moveKnight (c,r) = do
    (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c', r')

wMoveKnight :: Pos -> Path Pos
wMoveKnight pos = do
    tell [pos]
    pos' <- lift (moveKnight pos)
    return pos' 

instance Monad m => Monoid (a -> m a) where
    mappend = (>=>)
    mempty = return

test :: Int -> Pos -> Pos -> Path Pos
test n start end = do
    end' <- mconcat (replicate n wMoveKnight) start
    guard (end' == end)
    return end'

-- |
-- >>> runWriterT $ test 3 (6,1) (6,2)
