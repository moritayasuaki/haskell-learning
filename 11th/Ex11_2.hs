module Main where

import System.Environment
import Numeric
import qualified Data.ByteString as BS
import Data.List
import Data.Word

main :: IO ()
main = do
    [filename] <- getArgs
    byteData <- BS.readFile filename
    hexDump 16 $ BS.unpack byteData

hexDump :: Int -> [Word8] -> IO ()
hexDump _ [] = return ()
hexDump num dat | num < 0 = error "num must be positive."
hexDump num dat = do
    putStrLn $ concatMap (\w -> w2s w ++ " ") (take num dat)
    hexDump num (drop num dat)


-- |
-- >>> w2s 100
-- "64"

w2s :: Word8 -> String
w2s w | length hex <= 2 = take (2 - length hex) (repeat '0') ++ hex  
           | otherwise = "Word8 data is broken"
           where hex = showHex w ""
