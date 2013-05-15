module Ex12_2 where

import Text.Printf
import Text.Parsec.String(Parser)
import Text.Parsec(eof,char,(<?>),(<|>),many1,digit,parse,chainl1,chainr1,spaces,parseTest,string)
import Control.Applicative((*>),(<*),(<$>),(<*>),(<**>),pure,empty)
import Control.Monad((<=<),(=<<),(>>=),(>>),(>>=),ap)

data Expr = Atom Int
          | Op Char Expr Expr deriving (Show,Read)


token :: Parser a -> Parser a
token p = spaces *> p <* spaces

parens :: Parser Expr -> Parser Expr
parens p = token ( char '(' ) *> p <* token ( char ')' )

op :: Char -> Parser (Expr -> Expr -> Expr)
op c = token (Op <$> char c)


pInfixExpr :: Parser Expr
pInfixExpr = pInfixTerm `chainl1` pAdd

pInfixTerm :: Parser Expr
pInfixTerm = pInfixFactor `chainl1` pMul

pInfixFactor :: Parser Expr
pInfixFactor = parens pInfixExpr <|> pAtom

pMul :: Parser (Expr -> Expr -> Expr)
pMul = op '*' <|> op '/'

pAdd :: Parser (Expr -> Expr -> Expr)
pAdd = op '+' <|> op '-'

pAtom :: Parser Expr
pAtom = token (Atom <$> read <$> many1 digit)



-- |
-- >>> infixNToRpn "1 + 2 * 3"
-- "1 2 3 * +"
-- >>> infixNToRpn "1 * 2 + 3"
-- "1 2 * 3 +"
-- >>> infixNToRpn "1 * 2 - 3"
-- "1 2 * 3 -"
-- >>> infixNToRpn "( 1 + 2 )"
-- "1 2 +"
-- >>> infixNToRpn "( 1 + 2 ) * 3"
-- "1 2 + 3 *"
-- >>> infixNToRpn "10 - ( 4 + 3 ) * 2"
-- "10 4 3 + 2 * -"



infixNToRpn :: String -> String
infixNToRpn str = case parse pInfixExpr "infix arith" str of
                    Right expr -> showRpn expr
                    Left err -> error $ show err
-- |
-- >>> rpnToInfixN "1 2 3 * +"
-- "1 + 2 * 3"
-- >>> rpnToInfixN "1 2 * 3 +"
-- "1 * 2 + 3"
-- >>> rpnToInfixN "1 2 * 3 -"
-- "1 * 2 - 3"
-- >>> rpnToInfixN "1 2 +"
-- "1 + 2"
-- >>> rpnToInfixN "1 2 + 3 *"
-- "( 1 + 2 ) * 3"
-- >>> rpnToInfixN "10 4 3 + 2 * -"
-- "10 - ( 4 + 3 ) * 2"

rpnToInfixN :: String -> String
rpnToInfixN str = showInfix . readRpn $ str


readRpn :: String -> Expr
readRpn str = ll1 [] ls
    where ls = words str
          ll1 stk (l:ls) = 
            case parse pAtom "" l of
              Right a -> ll1 (a:stk) ls
              _ -> case parse pRpnOp "" l of
                     Right f -> ll1 (ans:(drop 2 stk)) ls
                             where ans = f n1 n0
                                   n0 = stk !! 0
                                   n1 = stk !! 1
                     _ -> error "error"
          ll1 [a] [] = a

-- pRpnExpr doesn't work.
-- I couldn't understand this behavior.

pRpnExpr :: Parser Expr
pRpnExpr = 
    do n <- pAtom
       calc <- pRpnCalc
       return $ calc n

pRpnCalc :: Parser (Expr -> Expr)
pRpnCalc = 
    (do rhs <- pRpnExpr
        op <- pRpnOp
        calc <- pRpnCalc
        return (calc . (`op` rhs))) <|>
    (do eof
        return id)

pRpnOp :: Parser (Expr -> Expr -> Expr)
pRpnOp = pMul <|> pAdd

showRpn :: Expr -> String
showRpn (Atom n) = printf "%d" n
showRpn (Op o l r) = printf "%s %s %c" (showRpn l) (showRpn r) o

showInfix :: Expr -> String
showInfix expr = showInfixWithRank 0 expr

showInfixWithRank :: Int -> Expr -> String
showInfixWithRank _ (Atom n) = printf "%d" n
showInfixWithRank n (Op '*' l r) = withParens (n > 1) $ printf "%s %c %s" (showInfixWithRank 1 l) '*' (showInfixWithRank 1 r)
showInfixWithRank n (Op '/' l r) = withParens (n > 1) $ printf "%s %c %s" (showInfixWithRank 1 l) '/' (showInfixWithRank 1 r)
showInfixWithRank n (Op '+' l r) = withParens (n > 0) $ printf "%s %c %s" (showInfixWithRank 0 l) '+' (showInfixWithRank 0 r)
showInfixWithRank n (Op '-' l r) = withParens (n > 0) $ printf "%s %c %s" (showInfixWithRank 0 l) '-' (showInfixWithRank 0 r)

withParens :: Bool -> String -> String
withParens True str = "( " ++ str ++ " )"
withParens False str = str



