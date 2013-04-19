module Main where

import System.Random
import System.Time
import Text.Printf

main :: IO ()
main = do
    secret <- mkSecret
    putStrLn "Hello, guess the number!"
    game secret

mkSecret :: IO String
mkSecret = do
    gen <- newStdGen
    let randChars = randomRs ('0','9') gen
    return $ take 4 randChars

game :: String -> IO ()
game secret = do
    putStr "> "
    input <- getLine
    case check secret input of
        InputError -> putStrLn "Please input 4 digits." >> game secret
        Incorrect (a,b) -> putStrLn (printf "%d %d" a b) >> game secret
        Correct -> putStrLn "Correct!!"

data Result = InputError
            | Incorrect (Int,Int)
            | Correct
            deriving Show

-- |
-- >>> check "1234" "1111"
-- Incorrect (1,1)
-- >>> check "1234" "2222"
-- Incorrect (1,1)
-- >>> check "1234" "3210"
-- Incorrect (1,3)
-- >>> check "1234" "0123"
-- Incorrect (0,3)
-- >>> check "1234" "1234"
-- Correct
-- >>> check "1234" "test"
-- InputError
-- >>> check "1234" "13"
-- InputError


check :: String -> String -> Result
check secret input
    | length input /= 4 || any (`notElem` ['0'..'9']) input
      = InputError
    | secret == input = Correct
    | otherwise = Incorrect (numOfMatched, numOfExists)
    where numOfMatched = 
            length . filter (== True) $ zipWith (==) secret input
          numOfExists = 
            length . filter (== True) $ map (`elem` input) secret