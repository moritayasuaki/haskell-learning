module Main where

import System.Environment
import Numeric
import qualified Data.ByteString as BS
import Data.List
import Data.Word

main :: IO ()
main = do
    [filename1, filename2] <- getArgs
    byteData1 <- BS.readFile filename1
    byteData2 <- BS.readFile filename2
    if byteData1 == byteData2
      then putStrLn "Same"
      else putStrLn "Different"

