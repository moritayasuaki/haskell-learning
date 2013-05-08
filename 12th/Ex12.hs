module Main where

import System.Environment
import Numeric
import qualified Data.ByteString as BS
import Data.Word
import Data.List

main :: IO ()
main = do
    [filename] <- getArgs
    byteData <- BS.readFile filename
    let words = BS.unpack byteData
    let dist = distOccur words
    print . take 10 . reverse . sort $ dist
    print dist
    let me = meanEntropy $ dist
    print me
    print $ me * fromIntegral (length words)

countOccur :: [Word8] -> [Int]
countOccur ws = map (flip count ws) symbols
    where count w = length . filter (w==)
          symbols = [0x00 .. 0xFF]

distOccur :: [Word8] -> [Double]
distOccur ws = map (\x -> fromIntegral x / fromIntegral (length ws)) (countOccur ws)

-- |
-- >>> distOccur [0x00,0x00,0x00,0x00,0x01,0x01,0xff,0xff]


meanEntropy :: [Double] -> Double
meanEntropy ps = sum (map plogp ps)
  where plogp 0 = 0.0
        plogp p = - p * log p
