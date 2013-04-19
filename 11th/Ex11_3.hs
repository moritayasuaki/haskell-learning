module Main where

import System.Environment
import qualified Data.ByteString as BS

main :: IO ()
main = do
    [filename1, filename2] <- getArgs
    byteData1 <- BS.readFile filename1
    byteData2 <- BS.readFile filename2
    putStrLn $ if byteData1 == byteData2 then "Same" else "Different"

