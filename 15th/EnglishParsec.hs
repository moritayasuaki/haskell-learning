module EnglishParsec where

import Data.Maybe
import Data.List
import Text.Parsec
import Text.Parsec.String(Parser)
import Control.Applicative hiding ((<|>),many)
import System.IO.UTF8 as U8

data S = S NP VP deriving Show
pS :: Parser S pS = S <$> pNP <*> pVP

data VP = Vi String
        | Vt String NP 
        | Vs String DS
        deriving Show

pVP :: Parser VP
pVP  =  (Vi <$> pVi)
    <|> (Vt <$> pVt <*> pNP)
    <|> (Vs <$> pVs <*> pDS)

data NP = NP (Maybe String) DN
        | Pronoun String
        deriving Show
pNP :: Parser NP
pNP  =  (Pronoun <$> pNpn)
    <|> (NP <$> optionMaybe (try pDet) <*> pDN)


data PP = PP String NP
        deriving Show
pPP :: Parser PP
pPP  = PP <$> pP <*> pNP

data AP = AP AdvP String
        deriving Show
pAP :: Parser AP
pAP = AP <$> pAdvP <*> pA

data DS = DS (Maybe String) S
        deriving Show
pDS :: Parser DS
pDS = DS <$> optionMaybe pRel <*> pS

data DN = DN [AP] String [PP]
        deriving Show
pDN :: Parser DN
pDN = DN <$> many (try pAP) <*> pN <*> many (try pPP)

data AdvP = AdvP [String]
          deriving Show
pAdvP = AdvP <$> many (try pAdv)

pWords ss = (choice $ map (try . string) ss) <* (many1 space)

pRel = pWords $ (fst . unzip) drel
drel = zip ["that","which","where","why","what"]
           ["そしてそれは","そしてそれは","そしてそれは","そしてそれは","そしてそれは"]

pN = pWords $ (fst . unzip) dnoun
dnoun = zip ["building","restaurant","apple"
           ,"paper","Haskell","code","Epson"
           ,"wood","man","woman","book"]
           ["建物","レストラン","りんご"
           ,"紙","ハスケル","コード","エプソン"
           ,"木","男","女","本"]

pDet = pWords $ (fst . unzip) ddet
ddet = zip ["every","the","many","a"]
           ["すべての","その","たくさんの","ひとつの"]

pNpn = pWords $ (fst . unzip) dpronoun
dpronoun = zip ["I","you","he","she","it","they","this","these","that","those"]
               ["私","あなた","彼","彼女","それ","彼ら","これ","これら","あれ","あれら"]

pVi = pWords $ (fst . unzip) dvi
dvi = zip ["laugh","smile","cry","walk","swim"]
          ["笑う","微笑む","泣く","歩く","泳ぐ"]

pVt = pWords $ (fst . unzip) dvt
dvt = zip ["love","like","hate","write","is","am","are","look","shot","sent","put"]
          ["愛する","好きだ","憎む","書く","である","である","である","見る","打つ","送る","置く"]

pVs = pWords $ (fst . unzip) dvs
dvs = zip ["think","believe","make"]
          ["考える","信じる","させる"]

pA = pWords $ (fst . unzip) da
da = zip ["great","good","bad","pretty","funny","beautiful","old","young"]
         ["すごい","いい","わるい","かわいい","面白い","美しい","年老いた","若い"]

pAdv = pWords $ (fst . unzip) dadv
dadv = zip ["really","always","quickly","slowly"]
           ["本当に","いつも","素早く","のろのろと"]
pP = pWords $ (fst . unzip) dp
dp = zip ["of","at","to","for","until","via","in","after","above","about"]
         ["の","にて","へ","のために","まで","を通して","の中で","の後で","の上の","について"]

-- |
-- >>> let text = "she love the old man "
-- >>> let Right test = parse (pS <* eof) "source" text
-- >>> let Just jtest = toJapanese test 
-- >>> Prelude.putStrLn jtest


toJapanese :: S -> Maybe String
toJapanese (S np vp) = do 
    jnp <- toJapaneseNP np
    jvp <- toJapaneseVP vp
    return $ jnp ++ "は" ++ jvp

toJapaneseNP (Pronoun pn) = lookup pn dpronoun
toJapaneseNP (NP Nothing dn) = toJapaneseDN dn
toJapaneseNP (NP (Just det) dn) = do
    jdet <- lookup det ddet
    jdn <- toJapaneseDN dn
    return $ jdet ++ jdn

toJapaneseVP (Vi vi) = lookup vi dvi
toJapaneseVP (Vt vt np) = do
    jvt <- lookup vt dvt
    jnp <- toJapaneseNP np
    return $ jnp ++ jvt
toJapaneseVP (Vs vs ds) = do
    jvs <- lookup vs dvs
    jds <- toJapaneseDS ds
    return $ jds ++ jvs


toJapanesePP (PP p np) = do
    jp <- lookup p dp
    jnp <- toJapaneseNP np
    return $ jp ++ jnp

toJapaneseAP (AP advp a) = do
    jadvp <- toJapaneseAdvP advp 
    ja <- lookup a da
    return $ jadvp ++ ja

toJapaneseDS (DS (Just that) s) = do
    js <- toJapanese s
    return $ that ++ js

toJapaneseDS (DS Nothing s) = do
    js <- toJapanese s
    return $ js

toJapaneseDN (DN aps n pps) = do
    let japs = concat (mapMaybe toJapaneseAP aps)
    jn <- lookup n dnoun
    let jpps = concat (mapMaybe toJapanesePP pps)
    return $ japs ++ jn ++ jpps

toJapaneseAdvP (AdvP advps) =
    return $ concat (mapMaybe (flip lookup dadv) advps)
