# 自作ParserCombinatorをモナドにしてみる
#### Morita Yasuaki
#### Aug 1, 2013

## Parserの型シグネチャ

```haskell
type Parser out = Source -> Either ErrorMessage (out, Source) 
```

入力を消費し、

パースできなければエラーメッセージを

パースできればパース結果と入力の残りを返す

## newtypeでinstance化できる形にする

``` haskell
type Parser out = Source -> Either ErrorMessage (out, Source)
```

type宣言ではParserを何かのinstanceにする事ができない。

下のようにnewtype宣言にする必要がある。

``` haskell
newtype Parser out = Parser { runParser :: Source -> Either ErrorMessage (out, Source) }
```

## 早速Monadにしてみる

```haskell
instance Monad Parser where
    m >>= f = ???
    return x = ???
```

- ???には一体何を書けばいいか考える。

## bindの定義を埋める 

- とにかく型が合うように関数を組み合わせる！

```haskell
m :: Parser a
f :: a -> Parser b
??? :: Parser b

m >>= f = ???
```

- mはParser型なのでパターンマッチで中身を取り出せる

```haskell
p :: Source -> Either ErrorMessage (a, Source)
f :: a -> Parser b
??? :: Parser b

(Parser p) >>= f = ???
```

- pの型がどうなってるかも頭に入れておく
- 返り値の型がParser bなのでParserコンストラクタを使う

```haskell
p :: Source -> Either ErrorMessage (a, Source)
f :: a -> Parser b
Parser :: (Source -> Either ErrorMessage (b, Source)) -> Parser b
??? :: Source -> Either ErrorMessage (b, Source)

(Parser p) >>= f = Parser ???
```


- ???の部分はSource -> Either ErrorMessage (b, Source)の型が必要
- 関数の型なのでλ式を使ってみる

```haskell
p :: Source -> Either ErrorMessage (a, Source)
f :: a -> Parser b
src :: Source
??? :: Either ErrorMessage (b, Source)

(Parser p) >>= f = Parser (\src -> ???)
```

- ???の部分にはEither ErrorMessage (b, Source)の型が必要
- b型に持ってくるにはfを使うしかない
- fを使うためにpとsrcを組み合わせてなんとかa型を取り出したい

```haskell
p :: Source -> Either ErrorMessage (a, Source)
f :: a -> Parser b
p src :: Either ErrorMessage (a, Source)
??? :: Either ErrorMessage (b, Source)

(Parser p) >>= f = Parser (\src -> case p src of
                              ???
                          )
```

- (p src)はEitherなのでLeftとRightでパターンマッチできる

```haskell
f :: a -> Parser b
p src :: Either ErrorMessage (a, Source)
err :: ErrorMessage
out :: a
src' :: Source
??? :: Either ErrorMessage (b, Source)

(Parser p) >>= f = Parser (\src -> case p src of
                                Left err -> ???
                                Right (out, src') -> ???
                          )
```

- Leftの場合は失敗をそのまま伝搬させる事にする
- a型の値outが取り出せたので(f out)が使える

```haskell
Left err :: Either ErrorMessage (b, Source)
src' :: Source
f out :: Parser b
??? :: Parser b -> Either ErrorMessage (b, Source)

(Parser p) >>= f = Parser (\src -> case p src of
                              Left err -> Left err
                              Right (out, src') -> ??? (f out)
                          )
```

- 型を合わせるために(f src) からEither(b,Source)の値を取り出したい
- runParserが使える

```haskell
src' :: Source
f out :: Parser b
runParser :: Parser b -> (Source -> Either ErrorMessage (b, Source))
runParser (f out) :: Source -> Either ErrorMessage (b, Source)
??? :: Source

(Parser p) >>= f = Parser (\src -> case p src of
                              Left err -> Left err
                              Right (out, src') -> runParser (f out) ???
                          )
```

- ここで使えるSource型の値はsrcかsrc'
- ちょっと実装の意味を考えてsrc'を与えるのが良さそう

```haskell
(Parser p) >>= f = Parser (\src -> case p src of
                              Left err -> Left err
                              Right (out, src') -> runParser (f out) src'
                          )
```

- これで完成
- 型が実装を導いてくれた

## returnの定義を埋める

```haskell
x :: a
??? :: Parser a

return x = ???
```

- 結果の型がParser aなのでParserコンストラクタを使う

```haskell
x :: a
Parser :: (Source -> Either ErrorMessage (a, Source)) -> Parser a
??? :: Source -> Either ErrorMessage (a, Source)

return x = Parser ???
```

- ???は関数の型なのでλ式使ってみる

```haskell
x :: a
src :: Source
??? :: Either ErrorMessage (a, Source)

return x = Parser (\src -> ???)
```

- returnはデフォルトの文脈なので余計な事をしない事が重要
- 入力は消費せず固定の値xを返すパーサにする

```haskell
x :: a
src :: Source

return x = Parser (\src -> Right (x,src))
```
## モナド則のチェック

- QuickCheck
- 手動展開
- 面倒なので今回は省略

```haskell
return x >>= f == f x
m >>= return == m
(m >>= f) >>= g == m >>= (\x -> f x >>= g)
```

## ApplicativeとFunctorの宣言

- Monadに出来ればあとは機械的にできる

```haskell

instance Applicative Parser where
    pure = return
    (<*>) = ap

instance Functor Parser where
    fmap = liftM
```

## まとめ

- instance宣言では実装よりも型が重要
- 型が実装を導く
- 値レベルのMonad則は自分でチェック
- Monadにできれば機械的にFunctor,Applicativeにできる
