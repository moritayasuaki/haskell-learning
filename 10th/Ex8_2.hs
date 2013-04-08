module Main (main, grep) where

import System.Environment
import System.FilePath
import Text.Regex.Posix
import Text.Printf


main = do
    [pat,path] <- getArgs
    contents <- readFile path
    let ts = grep pat contents
    putStr $ unlines . map (uncurry $ printf "%d:%s") $ ts


-- |
-- >>> grep "ch*" "change\ncatch\nsmash\nbach"
-- [(1,"change"),(2,"catch"),(4,"bach")]

grep :: String -> String -> [(Int,String)]
grep regex source =  
    [ (num, str) 
        | (num, str) <- zip [1 ..] ls
        , isMatched regex str ]
    where ls = lines source
          isMatched reg str = str =~ reg :: Bool
