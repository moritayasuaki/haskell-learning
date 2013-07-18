module ParserT where

import Control.Monad
import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Identity
type Message = String
type ParserT m = (StateT String) (ErrorT Message m) 
type Parser = ParserT Identity

runParser s = runIdentity . runErrorT . runStateT s

success :: Parser ()
success = return ()

char :: Char -> Parser Char
char c = do
    src <- get
    case src of
      c':cs  | c == c' -> put cs >> return c
      c':_   -> lift $ throwError ("expected: " ++ [c])
      _      -> lift $ throwError ("unexpected eof")

string :: String -> Parser String
string s = mapM char s <?> ("expected: " ++ s)

eof :: Parser ()
eof = do
    src <- get
    if src == "" then return () else lift $ throwError ("expected: eof")

parseIO :: Show a => Parser a -> String -> IO ()
parseIO p s =
    case runParser p s of
      Right (a,"") -> print a
      Right (a,s) -> print (a,s)
      Left m -> error m

(<?>) :: Parser a -> Message -> Parser a
p <?> mes = p <|> lift (throwError mes)

-- |
-- >>> parseIO (char 'h') "hello"

-- |
-- >>> parseIO (string "hello") "hello"
