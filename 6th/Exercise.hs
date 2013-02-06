--
-- Learning Haskell #6
-- Exercise
--
--   Morita Yasuaki   Feb 6, 2013
--

module Exercise where

import Control.Arrow
import BinNum
import HexNum
import DecNum
import OctNum

-- |
-- >>>  numToInt "10"
-- 10
-- >>>  numToInt "0b10"
-- 2
-- >>>  numToInt "0x10"
-- 16
-- >>>  numToInt "0o10"
-- 8

numToInt :: String -> Int
numToInt str
    | isBin str = binToInt str
    | isOct str = octToInt str
    | isDec str = decToInt str
    | isHex str = hexToInt str
    | otherwise = error $ str ++ " is not number expression"


bopNum :: (Int -> Int -> Int) -> String -> String -> Int
bopNum op a b = uncurry op . (numToInt *** numToInt) $ (a, b)

-- |
-- >>>  addNum "10" "10"
-- 20
-- >>>  addNum "0x10" "0b10"
-- 18
-- >>>  addNum "0o10" "0x10"
-- 24

addNum :: String -> String -> Int
addNum = bopNum (+)

-- |
-- >>>  subNum "10" "10"
-- 0
-- >>>  subNum "0x10" "0b10"
-- 14
-- >>>  subNum "0o10" "0x10"
-- -8

subNum :: String -> String -> Int
subNum = bopNum (-)

-- |
-- >>>  mulNum "10" "10"
-- 100
-- >>>  mulNum "0x10" "0b10"
-- 32
-- >>>  mulNum "0o10" "0x10"
-- 128

mulNum :: String -> String -> Int
mulNum = bopNum (*)

-- |
-- >>>  divNum "10" "10"
-- 1
-- >>>  divNum "0x10" "0b10"
-- 8
-- >>>  divNum "0o10" "0x10"
-- 0
-- >>>  divNum "0x10" "0o10"
-- 2

divNum :: String -> String -> Int
divNum = bopNum div

-- |
-- >>>  modNum "10" "10"
-- 0
-- >>>  modNum "0x10" "0o10"
-- 0
-- >>>  modNum "0x10" "0b101"
-- 1
-- >>>  modNum "0o10" "0x10"
-- 8

modNum :: String -> String -> Int
modNum = bopNum mod

-- |
-- >>>  showNum 10 $ addNum "0b11" "5"
-- "8"
-- >>>  showNum 2 $ addNum "0b11" "5"
-- "0b1000"
-- >>>  showNum 8 $ mulNum "0x10" "0b10"
-- "0o40"
-- >>>  showNum 8 $ divNum "0x10" "0o10"
-- "0o2"
-- >>>  showNum 16 $ modNum "0x11" "5"
-- "0x2"


showNum :: Int -> Int -> String
showNum 2 = intToBin
showNum 8 = intToOct
showNum 10 = intToDec
showNum 16 = intToHex
