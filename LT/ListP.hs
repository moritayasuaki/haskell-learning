module ListTagParser where

import Control.Applicative
import Test.QuickCheck
import Test.HUnit


-- |
-- 以下はパーサの使用例
main :: IO ()
main = do
    -- 正常なリストタグのパース(Rightが返ってくる)
    print $ runParser listHeader "<ol> <li> one </li> <li> two </li> <li> three </li> </ol>"
    -- 閉じタグをミスっているリストタグのパース(Leftが返ってくる)
    print $ runParser listHeader "<ol> <li> test <li> </ol>"


data Contents = S String | T ListTag deriving Show
data ListTag = UL [Contents] | OL [Contents] deriving Show


-- |
-- >>> runParser listHeader "<ul> <li> thanks </li> </ul>"
-- Right (T (UL [S "thanks"]),"")

-- |
-- 以下はリストパーサの実装

-- |
-- ulかolで囲まれた部分をパースするパーサ
listHeader :: Parser Contents
listHeader = (token "<ul>" *> ((T . UL) <$> listBody) <* token "</ul>")
          <|> (token "<ol>" *> ((T . OL) <$> listBody) <* token "</ol>")

-- |
-- liで囲まれた部分をパースするパーサ
listBody :: Parser [Contents]
listBody = many (token "<li>" *> listContents <* token "</li>")

listContents :: Parser Contents
listContents = (S <$> rawString) 
            <|> listHeader 

-- |
-- 文字列をパースするパーサ
rawString :: Parser String
rawString = tokenize $ many . choice $ map char (['a'..'z'] ++ ['A'..'Z'])

-- |
-- スペース区切られた部分の文字列をパースするコンビネータ
token :: String -> Parser String
token = tokenize . string

-- |
-- スペース読み飛ばし用のコンビネータ
tokenize :: Parser a -> Parser a
tokenize p = spaces *> p <* spaces

-- |
-- スペースを読み込むパーサ
spaces :: Parser String
spaces = many (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')

-- |
-- 以下はParsecの自前実装

type Source = String
type ErrorMessage = String

newtype Parser out = Parser { runParser :: Source -> Either ErrorMessage (out, Source) }


-- 型が合ってれば合ってるでしょ的なアレ
--
-- Monadなどのinstance化の中身の定義は
-- 読んでもよく分からない事が多い。
-- 書く方としては
-- 1. 使える手段が限られている(コンストラクタとcase,λ,$とかidとか.、型制約=>があればその情報も使える)
-- 2. 適合させるべき型が決まっている(class宣言で与えられるやつ)
-- という点を踏まえて、
-- 限られたピースを使ってパズルを解く感じで書いてる
-- 具体的な処理内容など無視して、とにかく型を合わせる

instance Monad Parser where
    (Parser p) >>= m = Parser $
        \src -> 
            case p src of 
                Left err -> Left err
                Right (out, rest) -> runParser (m out) rest 
    return = pure
    fail err = Parser $ \_ -> Left err -- これでいいのかな

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
              Left _ -> pa' src
              Right _ -> pa src
    empty = Parser $ \_ -> Left ""

-- |
-- >>> runParser (char 'p') "p"
-- Right ('p',"")
char :: Char -> Parser Char
char ch = Parser $ \src ->
            case src of
                []               -> Left "Unexpected end of file!"
                c:rest | c == ch -> Right (ch,rest)
                c:_              -> Left $ "A character " ++ [c] ++ " does't match " ++ [ch]

-- |
-- >>> runParser (string "test") "testable"
-- Right ("test","able")
string :: String -> Parser String
string (ch:str) = (:) <$> (char ch) <*> (string str)
string _ = Parser $ \s -> Right ([],s)

success :: Parser [a]
success = return []


-- |
-- >>> runParser (string "test" <|> string "tenis") "tent"
-- Left "A character t does't match i"

choice :: [Parser a] -> Parser a
choice ps = foldr1 (<|>) ps


-- |
-- 入力をキッチリ最後まで消費した場合に
-- パース結果を表示する関数
-- パース失敗で例外を返す
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s = case (runParser p $ s) of
                  Right (result,"") -> print result 
                  Right (_, rest) -> error (rest ++ " is unexpected!")
                  Left err -> error err
                  



-- |
-- Parserを色んなクラスのinstance化したので以下の関数は自前で定義しなくてもデフォルト実装が使える

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

