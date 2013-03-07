{-# OPTIONS -Wall -Werror #-}

module Ex7_7 where
import Test.HUnit
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import Test.QuickCheck
import Data.Time.Clock
import Control.DeepSeq
import System.Random


-- | ----------------------- 木を植えよう ---------------------------
-- | すごいHaskell 7.7 再帰的なデータ構造
-- | Tree型の定義
data Tree a = Empty | Node a (Tree a) (Tree a)
              deriving Show

-- | 要素をTreeに追加する
-- | すでに存在していたら、そのNodeをそのまま返すタイプの二分木
treeInsert :: (Ord a) => a -> Tree a -> Tree a 
treeInsert x Empty = Node x Empty Empty 
treeInsert x (Node a left right)
    | x == a    = (Node a left right)
    | x < a     = (Node a (treeInsert x left) right)
    | otherwise = (Node a left (treeInsert x right))

-- | 要素がTreeに含まれていればTrue
treeElem :: (Ord a) => a -> Tree a -> Bool 
treeElem _ Empty = False 
treeElem x (Node a left right)
    | x == a    = True
    | x < a     = treeElem x left
    | otherwise = treeElem x right

-- | [a]を空のTreeに追加する
list2Tree :: (Ord a) => [a] -> Tree a
list2Tree []   = Empty
list2Tree list = foldr treeInsert Empty list

-- | Treeを巡回して[a]を作る
tree2List :: Tree a -> [a]
tree2List Empty = []
tree2List (Node a left right) = (tree2List left) ++ [a] ++ (tree2List right)

-- | 上の定義を使って、
-- | 上の関数を使って、(または改良して使って、または使わずに）、次の問題を解いてください。

-- | 問題 7.7.1 Treeの高さを求める関数getHeightを作ってください。
getHeight :: Tree a -> Int
getHeight Empty = 0
getHeight (Node _ left right) = 1 + max (getHeight left) (getHeight right)

-- | 問題 7.7.2 Treeが二分平衡探索木のときにTrueを返す関数を実装してください。
-- | 二分平衡探索木とは、どのノードの２つの部分木も、その高さの差が１以下である
-- | ような木であると定義します。
-- | まず、「どのノードの２つの部分木も、その高さの差が１以下である
-- | ような木である」時にTrueを返す関数を作ってください。
isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Node _ left right) = 
    and [ isBalanced left
        , isBalanced right
        , abs (getHeight left - getHeight right) <= 1]


-- | 余裕のある人は次の問題も解いてみてください。
-- | 問題 7.7.3 isBalancedをさらに改良してください
-- | 左か右の子のいずれかが二分平衡探索木でなければ、もうそれ以外の木を探索する必要はありません。
-- | その場合は探索を打ち切ってただちにFalseを返すような関数を作ってください。


-- | ----------------------- アルゴリズム2 ---------------------------
-- | leftとrightのどちらかが平衡でなければ-1を返す
-- | そうでなければleftとrightの高い方+1を返す
shortCut :: Int -> Int -> Int
shortCut leftHeight rightHeight
    | leftHeight  == (-1) = (-1)
    | rightHeight == (-1) = (-1)
    | (abs $ leftHeight - rightHeight) > 1 = (-1)
    | otherwise           = max leftHeight rightHeight + 1
 
-- | Treeの高さを求める
-- | ただし、leftとrightのどちらかが平衡でなければ-1を返す
checkHeight :: Tree a -> Int
checkHeight Empty = 0
checkHeight (Node _ left right) 
            = shortCut leftHeight rightHeight
                       where leftHeight = checkHeight left
                             rightHeight = checkHeight right
 
-- | Treeが平衡だったらTrue
isBalanced' :: Tree a -> Bool
isBalanced' t = checkHeight t /= (-1)


-- | 
-- 木の高さを計算せずに局所的な木構造だけを見て判断する方法にした
-- QuickCheck, UnitTestレベルでは合ってそう

-- | 子ノードをリストにして返す関数

children :: Tree a -> [Tree a]
children Empty = []
children (Node _ l r) = [l,r]


-- |
-- 幅優先探索用関数
-- 条件propを満たす部分ノードを探す
-- リスト中にある木のいずれかが条件を満たせばTrue
-- そうでない場合は子要素達を取り出して再帰する

findInTrees :: (Tree a -> Bool) -> [Tree a] -> Bool
findInTrees _ [] = False
findInTrees prop trees 
    | any prop trees = True
    | otherwise     = findInTrees prop (concatMap children trees)


-- |
-- 明らかにアンバランスなパターンだとTrue
-- それ以外はアンバランスかどうかは不明という意味でのFalse
-- 明らかにアンバランス=一方は空、他方は孫ノードを持つ二分木
-- 曾孫の存在はDon't Care(チェックしない)
-- ↓こんなパターン
--  /    /    \    \
-- /     \    /     \

obviousInbalance :: Tree a -> Bool
obviousInbalance (Node _ Empty (Node _ _ (Node _ _ _))) = True
obviousInbalance (Node _ Empty (Node _ (Node _ _ _) _)) = True
obviousInbalance (Node _ (Node _ (Node _ _ _) _) Empty) = True
obviousInbalance (Node _ (Node _ _ (Node _ _ _)) Empty) = True
obviousInbalance _ = False


-- 幅優先で木をなめて明らかにアンバランスなパターンを見つける
-- パターンに合致するのがなければバランスしている？
-- UnitTestとかQuickCheckのレベルではオーケーっぽい

isBalanced'' :: Tree a -> Bool
isBalanced'' tree = not (findInTrees obviousInbalance [tree])


-- | 
-- ほぼ同じ事を深さ優先でやってみる

isBalanced''' :: Tree a -> Bool
isBalanced''' Empty = True
isBalanced''' (Node _ Empty Empty) = True
isBalanced''' (Node _ Empty (Node _ Empty Empty)) = True
isBalanced''' (Node _ (Node _ Empty Empty) Empty) = True
isBalanced''' (Node _ Empty (Node _ _ (Node _ _ _))) = False
isBalanced''' (Node _ Empty (Node _ (Node _ _ _) _)) = False
isBalanced''' (Node _ (Node _ (Node _ _ _) _) Empty) = False
isBalanced''' (Node _ (Node _ _ (Node _ _ _)) Empty) = False
isBalanced''' (Node _ l r) = isBalanced''' l && isBalanced''' r


-- |
-- QuickCheck用宣言
-- 50000nodeの2分木をランダム作成できるようにしとく
-- ほぼ非平衡木しか生成されないのであんまり便利じゃないかも

instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
    arbitrary = treeGenWithN 50000


treeGenWithN :: (Arbitrary a, Ord a) => Int -> Gen(Tree a)
treeGenWithN n = do
    ls <- vector n
    return (list2Tree ls)

-- | 性質テスト
prop_isBalanced' :: Tree Int -> Bool
prop_isBalanced' tree = 
    isBalanced' tree == isBalanced tree

prop_isBalanced'' :: Tree Int -> Bool
prop_isBalanced'' tree =
    isBalanced'' tree == isBalanced tree

prop_isBalanced''' :: Tree Int -> Bool
prop_isBalanced''' tree =
    isBalanced''' tree == isBalanced tree

-- | 性能テスト
-- ランダムに生成した引数データを20個渡して
-- 20回のそれぞれの時間を計測する
-- 正格評価とか遅延評価とかがあるため
-- 本当に計測したい部分がどこなのかよくわからん
-- (引数データ生成自体が遅延されてたりするので)

-- deepseq用宣言
instance NFData a => NFData (Tree a) where
    rnf Empty = ()
    rnf (Node a l r) = rnf a `seq` rnf l `seq` rnf r

-- 計算時間計測関数
time :: (NFData b) => (a -> b) -> a -> IO NominalDiffTime
time f x = do
    t1 <- getCurrentTime
    -- 強制評価。
    -- これやんないとf xが無駄な計算とみなされてしまう
    _ <- return $!! (f x) 
    t2 <- getCurrentTime
    return (diffUTCTime t2 t1)


-- strictな（全部評価済みの）データに対する実行時間
quickStats' :: (NFData a, NFData b, Arbitrary a) => (a -> b) -> IO [NominalDiffTime]
quickStats' f = do
    rnd <- newStdGen
    let g = unGen (vectorOf 30 arbitrary)
    let x = force $ g rnd 5 -- 引数xを全部評価済みにしておく
    stats <- sequence $ time f `map` x
    return stats

-- lazyな（必要でない部分は評価されていない）データに対する実行時間
-- 計測対象の関数がなめるデータに対して、
-- そのデータを生成する関数の実行時間も含める感じになる

quickStats :: (Arbitrary a, NFData b) => (a -> b) -> IO [NominalDiffTime]
quickStats f = do
    rnd <- newStdGen
    let g = unGen (vectorOf 30 arbitrary)
    let x = g rnd 5
    stats <- sequence $ time f `map` x
    return stats


-- |  ** quickStatsの使用例 **
-- quickStats isBalanced
-- quickStats isBalanced'
-- こんな感じで２つの関数の実行時間を比較できる(つもり)

-- |  やってみた結果
-- Nをノードの数とすると
-- list2Treeが慣らしでO(N log N)だと思う
-- データの作成にそれなりの時間がかかるようだ
--
-- isBalanced, isBalanced', isBalanced'', isBalanced'''
-- ランダムな木では慣らしでO(log N)とかそれ以下だと思う
-- 速すぎてあまり違いがわからん。
-- 実行時間的には
-- isBalanced'' = isBalanced' > isBalanced > isBalanced'''
-- という感じ
-- 計算量のオーダーというより
-- データアクセスとか値コンストラクタとかのオーバーヘッドの違いだと思う


-- |
-- コーナーケースでの処理時間計測用のデータ

-- 左に伸びる木
lTree :: Tree Int
lTree = list2Tree [0..1000]

-- 右に伸びる木
rTree :: Tree Int
rTree = list2Tree [1000,999..0]

-- 高さ19の平衡木
bTree :: Tree Int
bTree = mkBTree 19

mkBTree :: Int -> Tree Int
mkBTree 0 = Empty
mkBTree n = Node 0 (mkBTree (n-1)) (mkBTree (n-1))

-- |
-- ** 使用例 **
-- time isBalanced bTree
-- time isBalanced' rTree
-- などなど

-- |
-- 平衡木だと結果は結構変わる
-- 実行時間は
-- isBalanced > isBalanced' > isBalanced'' > isBalanced'''
-- か。
-- isBalanced'''はN回のパターンマッチ
-- isBalanced''は上に加えてリスト作成・連結のオーバーヘッド
-- isBalanced'は2N回のパターンマッチとN回のmax,1加算
-- isBalancedはパターンマッチが4Nとか6Nとかになる?maxとか1加算に
--             加えてリスト作成オーバーヘッドとか？
--    実測上は 100 : 20 : 10 : 3 ぐらいの比
-- 
--
-- 直線の木だと
-- isBalancedとisBalanced'はNがデカいと計算が終わらない。
--      本来はO(N)なはずなのでlist2TreeにO(N^2)かかってるのが原因だと思う。
--      実行時間は isBalanced' > isBalancedとなった
--      
-- isBalanced''とisBalanced'''は根のパターンマッチで終了する。O(1)
--      なぜか isBalanced'' > isBalanced''' になった。理由はわからない
--
-- 部分木の一部が無限に続いている非平衡木の場合、
-- isBalanced''だけが計算終了を保証できると思われる（試していない）


-- | ユニットテスト
balanceTest :: Test
balanceTest = test [
        (tree2List $ list2Tree "2013") ~=? ['0'..'3'],

        (getHeight Empty) ~=? 0,
        (getHeight (Node 'a' Empty Empty)) ~=? 1,
        (getHeight (Node 'a' (Node 'b' Empty Empty) Empty)) ~=? 2,
        (getHeight (Node 'a' (Node 'b' (Node 'c' Empty Empty) Empty) Empty)) ~=? 3,
        (getHeight (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty))) ~=? 2,

        (getHeight $ list2Tree ([]::[Int])) ~=? 0,
        (getHeight $ list2Tree ([1]::[Int])) ~=? 1,
        (getHeight $ list2Tree ([1,2]::[Int])) ~=? 2,
        (getHeight $ list2Tree ([1,3,2]::[Int])) ~=? 2,
        (getHeight $ list2Tree ([1,2,3]::[Int])) ~=? 3,
        (getHeight $ list2Tree ([1,3,5,7,9,11,13,15,2,6,10,14,4,12,8]::[Int])) ~=? 4,
        (getHeight $ list2Tree ([5,9,2,6,10,14,4,12,8]::[Int])) ~=? 4,
        (getHeight $ list2Tree ([5,9,6,10,14,4,12,8]::[Int])) ~=? 4,
        (getHeight $ list2Tree ([1..10]::[Int])) ~=? 10,

        (isBalanced Empty) ~=? True,
        (isBalanced (Node 'a' Empty Empty)) ~=? True,
        (isBalanced (Node 'a' (Node 'b' Empty Empty) Empty)) ~=? True,
        (isBalanced (Node 'a' (Node 'b' (Node 'c' Empty Empty) Empty) Empty)) ~=? False,
        (isBalanced (Node 'a' (Node 'b' Empty Empty) (Node 'c' Empty Empty))) ~=? True,

        (isBalanced $ list2Tree ([]::[Int])) ~=? True,
        (isBalanced $ list2Tree ([1]::[Int])) ~=? True,
        (isBalanced $ list2Tree ([1,2]::[Int])) ~=? True,
        (isBalanced $ list2Tree ([1,3,2]::[Int])) ~=? True,
        (isBalanced $ list2Tree ([1,2,3]::[Int])) ~=? False,
        (isBalanced $ list2Tree ([1,3,5,7,9,11,13,15,2,6,10,14,4,12,8]::[Int])) ~=? True,
        (isBalanced $ list2Tree ([5,9,2,6,10,14,4,12,8]::[Int])) ~=? True,
        (isBalanced $ list2Tree ([5,9,6,10,14,4,12,8]::[Int])) ~=? False,
        (isBalanced $ list2Tree ([1..10]::[Int])) ~=? False,

        True ~=? True
    ]

-- | main
main :: IO ()
main = do
      _ <- runTestTT balanceTest
      return ()

quickChecks :: IO ()
quickChecks = do
      _ <- quickCheck prop_isBalanced'
      _ <- quickCheck prop_isBalanced''
      _ <- quickCheck prop_isBalanced'''
      return ()


