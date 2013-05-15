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
    print . take 10 . reverse . sort $ dist -- 出現確率がでかいのを表示
    print dist -- 単純に分布を表示
    let me = meanEntropy $ dist
    print me -- 平均情報量
    print $ me * fromIntegral (length words) -- 平均情報量 * データ長さ

-- |
-- countOccur [0x00,0x01,0xff]
type Dict = [(Word8,Int)]
countOccur :: [Word8] -> [Int]
countOccur ws = map (\x -> count x ws) symbols
    where count w = length . filter (w==)
          symbols = [0x00 .. 0xFF]
-- |
-- >>> distOccur [0x00,0x00,0x00,0x00,0x01,0x01,0xff,0xff]


distOccur :: [Word8] -> [Double]
distOccur ws = map (\x -> fromIntegral x / fromIntegral (length ws)) (countOccur ws)


meanEntropy :: [Double] -> Double
meanEntropy ps = sum (map plogp ps)
  where plogp 0 = 0.0
        plogp p = - p * log p
