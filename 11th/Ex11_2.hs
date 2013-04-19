module Main where

import System.Environment
import Numeric
import qualified Data.ByteString as BS
import Data.Word

main :: IO ()
main = do
    [filename] <- getArgs
    byteData <- BS.readFile filename
    hexDump 16 $ BS.unpack byteData

hexDump :: Int -> [Word8] -> IO ()
hexDump _ [] = return ()
hexDump num _ | num < 0 = error "num must be positive."
hexDump num dat = do
    putStrLn $ concatMap (\w -> w2s w ++ " ") (take num dat)
    hexDump num (drop num dat)


-- |
-- >>> w2s 100
-- "64"

w2s :: Word8 -> String
w2s w | length hex <= 2 = replicate (2 - length hex) '0' ++ hex  
      | otherwise       = error "Word8 data is broken"
      where hex = showHex w ""
