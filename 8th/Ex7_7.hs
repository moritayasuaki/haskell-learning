{-# OPTIONS -Wall -Werror #-}

module Ex7_7 where
import Test.HUnit
--import Test.QuickCheck
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


-- | --------------------もっと速くなりそう-------------------

-- 子ノードをリストにして返す関数
children :: Tree a -> [Tree a]
children Empty = []
children (Node _ l r) = [l,r]

-- 幅優先探索用関数 
findInTrees :: (Tree a -> Bool) -> [Tree a] -> Bool
findInTrees _ [] = False
findInTrees prop trees 
    | any prop trees = True
    | otherwise     = findInTrees prop (concatMap children trees)

-- 明らかにアンバランスなパターンだとTrue
-- それ以外はアンバランスかどうかは不明という意味でのFalse
-- 明らかにアンバランス=一方は空、他方は孫ノードを持つ二分木
-- ひ孫の存在はDon't Care(チェックしない)
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
-- QuickCheckレベルではバランスしている模様
isBalanced'' :: Tree a -> Bool
isBalanced'' tree = (findInTrees obviousInbalance [tree])


-- | 性質テスト
prop_isBalanced' :: [Int] -> Bool
prop_isBalanced' ls =
    isBalanced' tree == isBalanced tree
    where tree = list2Tree ls

prop_isBalanced'' :: [Int] -> Bool
prop_isBalanced'' ls =
    isBalanced'' tree == isBalanced tree
    where tree = list2Tree ls

-- | 性能テスト
-- 性質としては絶対満たされるf x = f x の形でQuickCheckに渡し
-- 様々なxに対するfの性能を計測することにした
-- でもちゃんと計測するの難しい。
-- QuickCheckにもっと長いリストを生成してもらわないと。

stats_isBalanced :: [Int] -> Bool
stats_isBalanced ls =
    isBalanced tree == isBalanced tree
    where tree = list2Tree ls

stats_isBalanced' :: [Int] -> Bool
stats_isBalanced' ls =
    isBalanced' tree == isBalanced' tree
    where tree = list2Tree ls

stats_isBalanced'' :: [Int] -> Bool
stats_isBalanced'' ls =
    isBalanced'' tree == isBalanced'' tree
    where tree = list2Tree ls



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


