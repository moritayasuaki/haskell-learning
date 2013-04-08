{-# OPTIONS -Wall -Werror #-}

module Ex8_1 where
import Test.HUnit()
import Test.QuickCheck()
import Text.Printf

-- | 問題 8.1 何もしない関数 main1 を作ってください。
main1 :: IO ()
main1 = return ()

-- | 問題 8.2 以下の３つのI/Oアクションを使って、これらを上から順番に実行する関数を、
-- | do構文 を使ったものと、
-- | sequence関数 を使ったものと２通りで書いてください。
hello,myName,bye :: IO ()
hello  = putStrLn "Hello!"
myName = putStrLn "My name is Haskell."
bye    = putStrLn "Bye!"

main21 :: IO ()
main21 = do
    hello
    myName
    bye

main22 :: IO ()
main22 = sequence_ [hello, myName, bye]

-- | 問題 8.3 echoプログラムの終了
-- | "q"と入力されたら終了するプログラムを作りたいが動かない。
-- | ちゃんと動くようになおしてください。
main3 :: IO ()
main3 = do
  x <- getLine
  if x == "q"
    then return ()
    else do
      putStrLn x
      main3

-- | ----------------------- 九九の表 ---------------------------
-- | 問題 8.4.1 以下の手順にしたがって、以下のような九九の表を作って標準出力へ出力してください。
{-
 1  2  3  4  5  6  7  8  9 
 2  4  6  8 10 12 14 16 18 
 3  6  9 12 15 18 21 24 27 
 4  8 12 16 20 24 28 32 36 
 5 10 15 20 25 30 35 40 45 
 6 12 18 24 30 36 42 48 54 
 7 14 21 28 35 42 49 56 63 
 8 16 24 32 40 48 56 64 72 
 9 18 27 36 45 54 63 72 81 
-}

--　| まず、九九の一段ずつをいれた[[Int]]な配列、kuku2を作ってください。
kuku2 :: [[Int]]
kuku2 = [[a*b| a <- [1..9]] | b <- [1..9]]

-- | １つのIntを書式指定付きでStringにするには、Text.Printf.printf が使えます。
-- | printfという名前ですが、IO Stringを返すのではなく、Stringを返します。
is :: Int->String
is n = printf "%2d " n
  
--　| isを[Int]に適用して１行分の文字列を作ってください。
lis :: [Int]->String
lis ns = concatMap is ns

--　| 最後に lis と kuku2 を使って、九九の表を標準出力へ出力する関数 main41 を作ってください。
main41 :: IO ()
main41 = putStr $ unlines . map lis $ kuku2

-- | 問題 8.4.2 kuku2と、右畳込みfoldrを使って、
-- | 同じ九九の表を出力をする関数 main42 を作ってください。
main42 :: IO ()
main42 = putStr $ foldr (\l s -> lis l ++ "\n" ++ s) "" kuku2

-- | ----------------------- 合計と平均 ---------------------------
-- | 問題 8.5 数の統計情報を表示する
-- | ユーザーから空白区切りの整数の入力をもらって、
-- | 合計と平均を出力してください。
-- | 繰り返し入力とし、入力の終了条件は適当に決めてください。
{-
実行例
? 0 10 10
sum: 20
ave: 6.6666665
? 1 2 3 4 5 6 7 8 9 10
sum: 55
ave: 5.5
? 1
sum: 1
ave: 1.0
? 
-}

main5 :: IO ()
main5 = do
    putStr "? "
    l <- getLine
    let ns = map s2i . words $ l
    putStrLn $ printf "sum: %d" (sum ns)
    putStrLn $ printf "ave: %f" (i2d (sum ns) / i2d (length ns))
    where i2d = fromIntegral :: Int -> Double
          s2i = read :: String -> Int

-- | main
main :: IO ()
main = do
       main1
       main21
       main22
       main3
       main41
       main42
       main5
       return ()
