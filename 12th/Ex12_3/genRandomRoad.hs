module Main where

import System.Random

main :: IO ()
main = do
    gen <- getStdGen
    putStr . unlines . map show . take 750000 . randomRs (1::Int,100) $ gen
