module Plist where

import Data.List

type Source = String
type ErrorMessage = String

type Parser a = Source -> Either ErrorMessage (a, Source)

-- |
-- >>> char 'p' "p"
char :: Char -> Parser Char
char ch src = case src of
                []               -> Left "end of file!"
                c:rest | c == ch -> Right (ch,rest)
                c:_              -> Left $ [c] ++ " don't match " ++ [ch]

-- |
-- >>> string "test" "testable"
string :: String -> Parser String
string (ch:str) = (:) <$> (char ch) <*> (string str)
string _ = \s -> Right ([],s)

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> pa = \src -> case pa src of
              Left e -> Left e
              Right (ans, s) -> Right (f ans, s)

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
pab <*> pa = \src -> case pab src of
    Left e -> Left e
    Right (ans, s) -> case pa s of
        Left e -> Left e
        Right (ans2, s) -> Right (ans ans2, s)

(*>) :: Parser a -> Parser b -> Parser b
pa *> pb = \src -> case pa src of
    Left e -> Left e
    Right (ans, s) -> case pb s of
        Left e -> Left e
        Right (ans2, s) -> Right (ans2, s)

(<*) :: Parser a -> Parser b -> Parser a
pa <* pb = \src -> case pa src of
    Left e -> Left e
    Right (ans, s) -> case pb s of
        Left e -> Left e
        Right (ans2, s) -> Right (ans, s)



(<|>) :: Parser a -> Parser a -> Parser a
pa <|> pa' = \src -> case pa src of
    Left e -> pa' src
    Right _ -> pa src

-- |
-- >>> (string "abs" <|> string "abd") "abd"

-- |
-- >>> many (string "abs") "absabs"

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> success

success :: Parser [a]
success = \src -> Right ([],src)

anyChar :: Parser Char
anyChar c:cs = Right (c,cs)

data Contents = S String | T ListTag deriving Show
data ListTag = UL [Contents] | OL [Contents] deriving Show

listHeader = (string "<ul>" *> ((T . UL) <$> listBody) <* string "</ul>")
          <|> (string "<ol>" *> ((T . OL) <$> listBody) <* string "</ol>")

listBody = many (string "<li>" *> listContents <* string "</li>")

listContents = (S <$> rawString) 
            <|> listHeader 

rawString = string "hoge"

-- |
-- >>> listHeader "<ul><li>hoge</li></ul>" 
