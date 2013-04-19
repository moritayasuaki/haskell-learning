module Main where

import System.Random
import Text.Printf

lower :: Char
lower = '0'

upper :: Char
upper = '9'

len :: Int
len = 4

main :: IO ()
main = do
    secret <- mkSecret
    putStrLn "Hello, guess the number!"
    game secret

mkSecret :: IO String
mkSecret = do
    gen <- newStdGen
    return . take len $ randomRs (lower, upper) gen

game :: String -> IO ()
game secret = do
    putStr "> "
    input <- getLine
    case check secret input of
        InputError -> 
          putStrLn (printf "Please input %d digits." len) 
            >> game secret
        Incorrect a b -> 
          putStrLn (printf "%d %d" a b) 
            >> game secret
        Correct -> 
          putStrLn "Correct!"

data Result = InputError
            | Incorrect Int Int
            | Correct
            deriving Show

-- |
-- >>> check "1234" "1111"
-- Incorrect 1 1
-- >>> check "1234" "2222"
-- Incorrect 1 1
-- >>> check "1234" "3210"
-- Incorrect 1 3
-- >>> check "1234" "0123"
-- Incorrect 0 3
-- >>> check "1234" "1234"
-- Correct
-- >>> check "1234" "test"
-- InputError
-- >>> check "1234" "13"
-- InputError


check :: String -> String -> Result
check secret input
    | length input /= len || any (`notElem` [lower..upper]) input
                       = InputError
    | secret == input  = Correct
    | otherwise        = Incorrect numMatched numExists
    where numMatched = numTrue $ zipWith (==) secret input
          numExists = numTrue $ map (`elem` input) secret
          numTrue = length . filter (== True)
