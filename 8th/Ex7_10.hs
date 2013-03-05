{-# OPTIONS -Wall -Werror #-}

module Ex7_10 where
import Test.HUnit
--import Test.QuickCheck

-- | 問題 7.10.1 Functor型クラス
-- | p155「Map kがどのようにFunctor型クラスのインスタンスになるのか自力で突き止めることは読者への練習問題」
-- | を解いてください。
-- | MapはすでにFunctor型クラスのインスタンスであり、fmapは定義済みなので、
-- | 新たにfmap'を定義して、fmapと同じように動くことを確認してください。

import qualified Data.Map as Map

fmap' :: (Ord k)=>(a->b)->Map.Map k a->Map.Map k b 
fmap' f = Map.fromAscList . map ( \(key,value) -> (key,f value) ) . Map.toAscList

-- | ユニットテスト
mapFmapTest :: Test
mapFmapTest = test [
        Map.toList map1 ~=? ([('a',"alpha"),('b',"beta")]::[(Char,String)]),
        fmap' length map1 ~=? Map.fromList ([('a',5),('b',4)]::[(Char,Int)]),
        fmap' length map1 ~=? fmap length map1,
        True ~=? True
    ] where map1 = Map.fromList [('a',"alpha"),('b',"beta")]

-- | main
main :: IO ()
main = do
       let map1 = Map.fromList [('a',"alpha"),('b',"beta")]
       print map1
       print $ fmap length map1
       print $ fmap' length map1
       _ <- runTestTT mapFmapTest
       return ()

 
