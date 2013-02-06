--クイックソート
--quicksort :: (Ord a) => [a] -> [a]
--quicksort [] = []
--quicksort (x : xs) = 
--    let
--        smallerOrEqual = [a | a <- xs, a <= x]
--        larger = [a | a <- xs, a > x]
--    in
--        quicksort smallerOrEqual ++ [x] ++ quicksort larger
--main = print(quicksort [2,1,4,3,6,5])

--quicksort :: (Ord a) => [a] -> [a]
--quicksort [] = []
--quicksort (x : xs) = quicksort smallerOrEqual ++ [x] ++ quicksort larger
--    where
--        smallerOrEqual = [a | a <- xs, a <= x]
--        larger = [a | a <- xs, a > x]
--main = print(quicksort [2,1,4,3,6,5])


--100以上999以下の３桁の自然数において百の位と一の位の数字が等しい数はいくつ
--main = print(length [(a,b,c) | a <- [1..9], b <- [0..9], c <- [0..9], a == c])

--以下のプログラムの２つのエラーを修正(Hugsでは3つのエラー)
--N = a ‘div’ length xs
--    where
--        a = 10
--        xs = [1, 2, 3, 4, 5]
--main = print(N)
--関数名は大文字から始めない
--シングルコーテーションではなくバックコーテーション
--Hugsではlength xsに()が必要?・・・申し訳ございません未確認です
--n =  a `div` (length xs)
--    where
--        a = 10
--        xs = [1, 2, 3, 4, 5]
--main = print(n)


--フィボナッチ数列のn番目を返す(遅い)
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib ( n - 2)
--main = print(fib 100)


--リストの要素を全て足す
--mySum :: [Int] -> Int
--mySum [] = 0
--mySum (x:xs) = x +  mySum xs
--main = print(mySum [1,2,3,4])


--リストの要素を偶数の値と奇数の値のリストに分ける
--oddEven :: [Int] -> ([Int], [Int])
--oddEven [] = ([], [])
--oddEven (x:xs)
-- | odd x = connectTuple ([x], []) (oddEven xs)
-- | otherwise = connectTuple ([], [x]) (oddEven xs)
--    where
--        connectTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
--        connectTuple (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
--main = print(oddEven [1,2,3,4,5,6])

--oddEven :: [Int] -> ([Int], [Int])
--oddEven [] = ([], [])
--oddEven (x:xs)
-- | x `mod` 2 == 0 = connectTuple ([x], []) (oddEven xs)
-- | otherwise = connectTuple ([], [x]) (oddEven xs)
--    where
--        connectTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
--        connectTuple (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
--main = print(oddEven [1,1,2,2,3,3,4,4])


--リストの要素を偶数番目と奇数番目のリストに分ける
--oddEven :: [Int] -> ([Int], [Int])
--oddEven [] = ([], [])
--oddEven [x] = ([x], [])
--oddEven (x:xs:xss)
-- | length xss == 1 = connectTuple ([x], [xs]) (xss, [])
-- | otherwise = connectTuple ([x], [xs]) (oddEven xss)
--    where
--        connectTuple :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
--        connectTuple (x1, y1) (x2, y2) = (x1 ++ x2, y1 ++ y2)
--main = print(oddEven [1,1,2,2,3,3,4])


--ランレングス解凍
--unrunlength :: [[Int]] -> [Int]
--unrunlength [] = []
--unrunlength (x:xs) = (unpack(head x)(last x)) ++ unrunlength xs
--    where
--        unpack::Int -> Int -> [Int]
--        unpack 0 _ = []
--        unpack n n2 = [n2] ++ unpack (n-1) n2
--main = print(unrunlength [[3,1],[1,0],[1,1],[4,0],[1,1]])


--log2の整数を取る
--log2 :: Int -> Int
--log2 0 = 0
--log2 1 = 0
--log2 n =1 + log2 ( n `div` 2 )
--main = print(log2 3)




