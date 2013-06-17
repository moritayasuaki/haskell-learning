module NumParsec where

import Text.Parsec
import Text.Parsec.String(Parser)
import Control.Applicative hiding ((<|>))

-- | 数字の連続のパース
-- >>> parse (pDigits <* eof) "test1" "12345"
-- Right "12345"

-- | カンマがあるのでパースエラー >>> parse (pDigits <* eof) "test2" "12345.6789"
-- Left "test2" (line 1, column 6):
-- unexpected '.'
-- expecting digit or end of input

pDigits :: Parser String
pDigits = many1 digit

-- | カンマのパース
-- >>> parse (pComma <* eof) "comma" "."
-- Right '.'

pComma :: Parser Char
pComma = char '.'

-- | 整数のパース
-- >>> parse (pInt <* eof) "int" "12345"
-- Right 12345

pInt :: Parser Int
pInt =  read <$> pDigits

-- | 小数のパース
-- >>> parse (pDouble <* eof) "flac1" "1234.5678"
-- Right 1234.5678

-- | 小数のパース
-- >>> parse (pDouble' <* eof) "flac2" "1234"
-- Left "flac2" (line 1, column 5):
-- unexpected end of input
-- expecting digit or "."

-- | 小数のパース
-- >>> parse (pDouble <* eof) "flac2" "1234"
-- Right 1234.0

-- カンマが無いとパースしてくれないバージョン
pDouble' :: Parser Double
pDouble' = do
    intPart <- pDigits
    comma <- pComma
    flacPart <- pDigits
    return $ read ( intPart ++ [comma] ++ flacPart)

-- カンマが無くてもパースしてくれるバージョン
pDouble :: Parser Double
pDouble = do
    intPart <- pDigits
    optionalPart <- option "" (
        do comma <- pComma
           flacPart <- pDigits
           return (comma:flacPart)
           )
    return $ read ( intPart ++ optionalPart )


