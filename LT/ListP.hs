module ListTagParser where

data Contents = S String | T ListTag deriving Show
data ListTag = UL [Contents] | OL [Contents] deriving Show

-- |
-- 以下はリストパーサの自前実装

-- |
-- >>> listHeader "<ul> <li> thanks </li> </ul>" 

listHeader = (token "<ul>" *> ((T . UL) <$> listBody) <* token "</ul>")
          <|> (token "<ol>" *> ((T . OL) <$> listBody) <* token "</ol>")

listBody = many (token "<li>" *> listContents <* token "</li>")

listContents = (S <$> rawString) 
            <|> listHeader 

rawString = tokenize $ many . choice $ map char (['a'..'z'] ++ ['A'..'Z'])

token = tokenize . string

tokenize p = spaces *> p <* spaces


-- |
-- 以下はParsecの自前実装

type Source = String
type ErrorMessage = String

type Parser output = Source -> Either ErrorMessage (output, Source)

-- |
-- >>> char 'p' "p"
-- Right ('p',"")
char :: Char -> Parser Char
char ch src = case src of
                []               -> Left "end of file!"
                c:rest | c == ch -> Right (ch,rest)
                c:_              -> Left $ [c] ++ " don't match " ++ [ch]

-- |
-- >>> string "test" "testable"
-- Right ("test","able")
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



