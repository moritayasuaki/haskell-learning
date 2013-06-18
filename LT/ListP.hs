module Plist where

import Data.List

data Contents = S String | T ListTag deriving Show
data ListTag = UL [Contents] | OL [Contents] deriving Show

-- |
-- >>> listHeader "<ul><li>thanks</li></ul>" 

listHeader = (string "<ul>" *> ((T . UL) <$> listBody) <* string "</ul>")
          <|> (string "<ol>" *> ((T . OL) <$> listBody) <* string "</ol>")

listBody = many (string "<li>" *> listContents <* string "</li>")

listContents = (S <$> rawString) 
            <|> listHeader 

rawString = many . choice $ map char (['a'..'z'] ++ ['A'..'Z'])


type Source = String
type ErrorMessage = String

type Parser output = Source -> Either ErrorMessage (output, Source)

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

many :: Parser a -> Parser [a]
many p = (:) <$> p <*> many p <|> success

success :: Parser [a]
success = \src -> Right ([],src)

choice :: [Parser a] -> Parser a
choice ps = foldr1 (<|>) ps

spaces :: Parser String
spaces = many (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')



