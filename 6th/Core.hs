--
-- Learning Haskell #6
-- Exercise
--
--   Morita Yasuaki   Feb 6, 2013
--

module Core where


import Data.Maybe
import Data.List



-- |
--
-- Corner case!
-- >>> toNAryList 2 0
-- []
-- 
-- >>> toNAryList 2 10
-- [0,1,0,1]
--
-- >>> toNAryList 0 10
-- *** Exception: can't express 0-ary number
--
-- prop> x > 1 ==> toNAryList x x == [0,1]
--                     where types = x :: Int


toNAryList :: Integral i => Int -> i -> [Int]
toNAryList n m 
    | n < 1 = error $ "can't express " ++ n' ++ "-ary number"
        where n' = show n
toNAryList n 0 = []
toNAryList n m = fromIntegral (rem m n') : (toNAryList n $ quot m n')
        where n' = fromIntegral n

-- |
--
-- >>> fromNAryList 2 [0,1,0,1]
-- 10
--
-- prop> b > 1 ==> (fromNAryList b . toNAryList b $ x) == x
--                      where types = (b,x) :: (Int,Int)

fromNAryList :: Integral i => Int -> [Int] -> i
fromNAryList n ms = sum $ zipWith (\rank m -> (fromIntegral m) * n' ^ rank ) [0..] ms
        where n' = fromIntegral n

-- |
-- 
-- >>> lookup 0 i2d
-- Just '0'
--
-- >>> lookup 1 i2d
-- Just '1'
--
-- >>> lookup 15 i2d
-- Just 'f'
-- 
-- >>> lookup 35 i2d
-- Just 'z'
--
-- >>> lookup '0' d2i
-- Just 0
--
-- >>> lookup 'a' d2i
-- Just 10
--
-- >>> lookup 'A' d2i
-- Just 10

ilist :: [Int]
ilist = [0..]

dlist :: [Char]
dlist = ['0'..'9'] ++ ['a'..'z']

dlist' :: [Char]
dlist' = ['0'..'9'] ++ ['A'..'Z']

i2d :: [(Int,Char)]
i2d = zip ilist dlist

d2i :: [(Char,Int)]
d2i = zip dlist ilist `union` zip dlist' ilist

binPrefix :: String
binPrefix = "0b"
octPrefix :: String
octPrefix = "0o"
decPrefix :: String
decPrefix = ""
hexPrefix :: String
hexPrefix = "0x"
dhexPrefix :: String
dhexPrefix = "0dx" -- experimental

-- |
-- 
-- >>> intToBin 0
-- "0b0"
--
-- >>> intToBin 1
-- "0b1"
--
-- >>> intToBin 10
-- "0b1010"
--
-- >>> intToBin (-10)
-- "-0b1010"

intToNAry :: Integral i =>  Int -> String -> i -> String
intToNAry n prefix m
  | m > 0      = prefix ++ (convert $ toNAryList n m)
  | m  == 0    = prefix ++ "0"                 -- !!corner case!!
  | otherwise  = "-" ++ intToNAry n prefix (-m)
  where convert = mapMaybe (\i -> lookup i i2d) . reverse


intToBin :: Int -> String
intToBin = intToNAry 2 binPrefix

intToOct :: Int -> String
intToOct = intToNAry 8 octPrefix

intToDec :: Int -> String
intToDec = intToNAry 10 decPrefix

intToHex :: Int -> String
intToHex = intToNAry 16 hexPrefix

integerToDhex :: Integer -> String
integerToDhex = intToNAry 32 dhexPrefix


-- nAryToInt support minus sign
nAryToInt :: Integral i => Int -> String -> String -> i
nAryToInt n prefix str 
  | isMinus str = negate . nAryToInt n prefix . dropMinus $ str
  | isNAry n prefix str = fromNAryList n . convert . dropPrefix prefix $ str
  where convert = mapMaybe (\d -> lookup d d2i) . reverse

nAryToInt n _ _ = error $ n' ++ "-Ary digits format error"
  where n' = show n

-- | binToInt support minus sign
-- 
-- >>> binToInt "0b0"
-- 0
--
-- >>> binToInt "0b1"
-- 1
--
-- >>> binToInt "0b10"
-- 2
--
-- >>> binToInt "0b1010"
-- 10
--
-- >>> binToInt "-0b1111"
-- -15
--
-- prop> (binToInt . intToBin $ x) == x

binToInt :: String -> Int
binToInt = nAryToInt 2 "0b"

octToInt :: String -> Int
octToInt = nAryToInt 8 "0o"

decToInt :: String -> Int
decToInt = nAryToInt 10 ""

hexToInt :: String -> Int
hexToInt = nAryToInt 16 "0x"

-- |
--
-- >>> dhexToInteger "0dxSEIKOEPSON"
-- 1001196352238359
--
-- can't pass 'Y'...
--
-- >>> dhexToInteger "0dxMoritaYasuaki"
-- *** Exception: 32-Ary digits format error
--
-- >>> dhexToInteger "0dxShimaToshihiro"
-- 1053291006435189640056


dhexToInteger :: String -> Integer
dhexToInteger = nAryToInt 32 "0dx"  -- experimental


-- |
--
-- >>> isBin "0b0"
-- True
--
-- >>> isBin "0b10"
-- True
--
-- >>> isBin "10"
-- False
--
-- >>> isBin "0a10"
-- False


isNAry :: Int -> String -> String -> Bool
isNAry n prefix str 
  | isPrefixOf prefix str = 
      case invalidChars of
        [] -> True
        _  -> False
  | otherwise = False
  where body = dropPrefix prefix str
        acceptables = take n dlist `union` take n dlist'
        invalidChars = filter (\d -> notElem d acceptables) body


isMinus :: String -> Bool
isMinus ('-':str) = True
isMinus _         = False

dropMinus :: String -> String
dropMinus ('-':str) = str
dropMinus str         = error $ str ++ " has not minus prefix"

dropPrefix :: String -> String -> String
dropPrefix prefix str | isPrefixOf prefix str = drop (length prefix) str
                      | otherwise = error $ str ++ " don't have prefix " ++ prefix

isBin :: String -> Bool
isBin = isNAry 2 binPrefix

isOct :: String -> Bool
isOct = isNAry 8 octPrefix

isDec :: String -> Bool
isDec = isNAry 10 decPrefix

isHex :: String -> Bool
isHex  = isNAry 16 hexPrefix

isDhex :: String -> Bool
isDhex = isNAry 32 dhexPrefix


-- ^
-- >>> intToDec 0
-- "0"
-- >>> intToDec 10
-- "10"
-- >>> intToDec 100
-- "100"
-- >>> decToInt "0"
-- 0
-- >>> decToInt "10"
-- 10
-- >>> decToInt "100"
-- 100
-- >>> isDec "0"
-- True
-- >>> isDec "0b01"
-- False
-- >>> isDec "100"
-- True
-- >>> isDec "0x100"
-- False
-- >>>  intToHex 0
-- "0x0"
-- >>>  intToHex 10
-- "0xa"
-- >>>  intToHex 100
-- "0x64"
-- >>>  hexToInt "0x0"
-- 0
-- >>>  hexToInt "0xA"
-- 10
-- >>>  hexToInt "0x64"
-- 100
-- >>>  isHex "0"
-- False
-- >>>  isHex "0x0"
-- True
-- >>>  isHex "0b0"
-- False
-- >>>  isHex "0x64"
-- True
-- >>>  intToOct 0
-- "0o0"
-- >>>  intToOct 10
-- "0o12"
-- >>>  intToOct 100
-- "0o144"
-- >>>  octToInt "0o0"
-- 0
-- >>>  octToInt "0o12"
-- 10
-- >>>  octToInt "0o144"
-- 100
-- >>>  isOct "0o0"
-- True
-- >>>  isOct "0"
-- False
-- >>>  isOct "0x0"
-- False
-- >>>  isOct "0o100"
-- True
