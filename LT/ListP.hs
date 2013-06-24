{-# LANGUAGE FlexibleInstances, TypeFamilies, TypeSynonymInstances #-}

module ListTagParser where

import Control.Monad
import Control.Applicative

data Contents = S String | T ListTag deriving Show
data ListTag = UL [Contents] | OL [Contents] deriving Show

-- |
-- 以下はリストパーサの自前実装

-- |
-- >>> runParser listHeader "<ul> <li> thanks </li> </ul>"
-- Right (T (UL [S "thanks"]),"")


listHeader = (token "<ul>" *> ((T . UL) <$> listBody) <* token "</ul>")
          <|> (token "<ol>" *> ((T . OL) <$> listBody) <* token "</ol>")

listBody = many (token "<li>" *> listContents <* token "</li>")

listContents = (S <$> rawString) 
            <|> listHeader 

rawString = tokenize $ many . choice $ map char (['a'..'z'] ++ ['A'..'Z'])

token = tokenize . string

tokenize p = spaces *> p <* spaces

spaces = many (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')

-- |
-- 以下はParsecの自前実装

type Source = String
type ErrorMessage = String

newtype Parser out = Parser { runParser :: Source -> Either ErrorMessage (out, Source) }

-- 型が合ってれば合ってるでしょ的なあれ
instance Monad Parser where
    (Parser p) >>= m = Parser $
        \src -> 
            case p src of 
                Left err -> Left err
                Right (out, rest) -> runParser (m out) rest 
    return = pure
    fail err = Parser $ \src -> Left err -- これでいいのかな

instance Applicative Parser where
    pure out = Parser $ \src -> Right (out, src)
    p1 <*> p2 = do p1out <- p1
                   p2out <- p2
                   return (p1out p2out)

instance Functor Parser where
    fmap f (Parser p) = Parser $ 
        \src -> case p src of
                    Left err -> Left err
                    Right (out, rest) -> Right (f out, rest)

instance Alternative Parser where
    Parser(pa) <|> Parser(pa') = Parser $
        \src -> 
            case pa src of
              Left e -> pa' src
              Right _ -> pa src
    empty = Parser $ \src -> Left ""

-- |
-- >>> runParser (char 'p') "p"
-- Right ('p',"")
char :: Char -> Parser Char
char ch = Parser $ \src ->
            case src of
                []               -> Left "end of file!"
                c:rest | c == ch -> Right (ch,rest)
                c:_              -> Left $ [c] ++ " don't match " ++ [ch]

-- |
-- >>> runParser (string "test") "testable"
-- Right ("test","able")
string :: String -> Parser String
string (ch:str) = (:) <$> (char ch) <*> (string str)
string _ = Parser $ \s -> Right ([],s)


{- (*>) :: Parser a -> Parser b -> Parser b -}
{- pa *> pb = \src -> case pa src of -}
    {- Left e -> Left e -}
    {- Right (ans, s) -> case pb s of -}
        {- Left e -> Left e -}
        {- Right (ans2, s) -> Right (ans2, s) -}

{- (<*) :: Parser a -> Parser b -> Parser a -}
{- pa <* pb = \src -> case pa src of -}
    {- Left e -> Left e -}
    {- Right (ans, s) -> case pb s of -}
        {- Left e -> Left e -}
        {- Right (ans2, s) -> Right (ans, s) -}

{- many :: Parser a -> Parser [a] -}
{- many p = (:) <$> p <*> many p <|> success -}

success :: Parser [a]
success = return []


-- |
-- >>> runParser (string "test" <|> string "tenis") "tent"
-- Left "t don't match i"

choice :: [Parser a] -> Parser a
choice ps = foldr1 (<|>) ps

