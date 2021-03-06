Parsec 入門 
===========


Recursive Decent Parsing &mdash; 再帰下降構文解析
-------------------------------------------------

再帰下降構文解析とは構文を上から舐めて行くタイプのパーサです。

例えば以下のようなulとolとliの三つのタグからなるhtmlがあるとします。

`ブラウザでの表示例`

<div style="border-style:dashed">
<ul>
  <li> Language
    <ul>
      <li> English </li>
      <li> Japanese </li>
    </ul>
  </li>
  <li> Locale
    <ol>
      <li> First </li>
      <li> Second </li>
      <li> Third </li>
    </ol>
  </li>
</ul>
</div>

`html`

```html
<ul>
  <li> Language
    <ul>
      <li> English </li>
      <li> Japanese </li>
    </ul>
  </li>
  <li> Locale
    <ol>
      <li> First </li>
      <li> Second </li>
      <li> Third </li>
    </ol>
  </li>
</ul>
```

上のような文は以下のような構文を持つものとしてパースすることができます。

`EBNF記法`

```
listheader = "<ul>", listbody, "</ul>" | "<ol>", listbody, "</ol>" ;
listbody = { "<li>", listcontents, "</li>" } ;
listcontents = rawstring | listheader
```

上ではrawstringの定義を抜いていますが
ここではアルファベットからなる文字列とします。

また空白の構文記述は省略しています。

再帰下降構文解析は次のような呼び出し順序になります

+ listheader全体を処理する関数を呼び出される
+ 上記関数の中でlistbodyの繰り返しを処理する関数が呼び出される
+ 上記関数の中でrawstringを処理する関数が呼び出され、必要に応じてlistheaderを処理する関数が再帰呼び出しされる

このように再帰下降パーサはEBNF記法の定義がほぼそのまま関数に対応する形になります。

逆にボトムアップ構文解析では
はじめに`<`を受理する状態になり、
次に`u`もしくは`o`を受理する状態になり、、
といったステートマシンの動作記述に主眼を置く形になるのかなと思います。

Parser Combinator &mdash; パーサコンビネータ
--------------------------------------------

haskellのように関数がファーストクラスである言語と
再帰下降構文解析は非常に相性がいいです。

小さなパーサや高階関数の組み合わせで
いろいろなパーサが作れるからです。

パーサを「入力文字列を先頭から消費して、内容に応じて何らかの出力結果を返す関数」と考えます。

Haskellではパーサはどんな型になるか考えてみます。

`単純な型宣言`

```haskell
-- 引数のStringは入力文字列を表し
-- 返り値のaはパーサの出力の型を表す
type Parser a = String -> a
```

これでも良さそうですが、
「入力文字列を消費する」という作用を表現できません。

とくにHaskellでは副作用を記述できません！

そこで「入力文字列を消費する」の代わりに
「出力と同時に食い残しの文字列も一緒に返す」事で対応します。

`入力の消費を表す型宣言`

```haskell
-- 引数のStringは入力文字列
-- aはパーサの出力の型
type Parser a = String -> (a, String)
```

こうしておくと前段のパーサが食い残した文字列を
後続のパーサに食わせる、というように連鎖させる事ができます。
実はこれは後で学ぶStateモナドと同じ形をしています。

もう一つ考える事があります。
それはパースの失敗です。

gccにhaskellのコードを食わせてもシンタックスエラーが起こるように、
規定の構文に従わない物は受付けようがありません。

これは失敗の可能性を表すMaybeやEitherが良さそうです。

Maybeでもいいのですが、
一応なぜパース失敗が起きたか、後で知れるように
Eitherにしておきましょう。

`失敗を表す型宣言`

```haskell
type Parser a = String -> Either String (a, String)
```

なんかややこしくなってきたので、
もうちょっと意味が分かりやすい型シグネチャにしてみましょうか。

単に名前を付け替えただけで本質的には何も変わっていません。

`名前だけ変えた型宣言`

```haskell
type Source = String
type ErrorMessage = String

-- output型を出力するパーサは
-- 入力文字列を取って、パース失敗したらエラーメッセージを、
-- パース成功なら出力結果と入力の食い残しを返す
type Parser output = Source -> Either ErrorMessage (output, Source)
```

Either(Maybeでも可)を使う事で「失敗を表す」だけでなくもう一つ利点が加わります。

それはあるパーサでパースに失敗した時に、別のパーサでトライするという
パーサの組み合わせ方ができるという事です。

これはEitherやMaybeがAlternativeであるという事と直接的に関連しています。

以上がパーサの基本形です。

Haskellのライブラリの`Text.Parsec`ライブラリは
高機能かつ汎用的な設計なので、ややこしいシンタックスになっていますが、
大前提となる部分は同じです。（たぶん）

Combinators &mdash; 代表的なパーサコンビネータ
-------------------------------------------

原理は分かりましたが、
大きなパーサを組み上げるためには
もう少し部品となるパーサを用意しておくと便利です。

`Text.Parsec`にも標準で入っている
代表的な部品を挙げます。

### char :: Char -> Parser Char ###

特定の文字のみ受け付けるパーサ

例えば

```haskell
parseTest (char 'h') "h" -- パース成功
parseTest (char 'h') "a" -- パース失敗
```

のように動作します

### string :: String -> Parser String ###

特定の文字列のみ受け付けるパーサ

例えば

```haskell
parseTest (string "hello") "hello" -- パース成功
parseTest (string "hello") "world" -- パース失敗
```

のように動作します

### many :: Parser a -> Parser [a] ###

繰り返しをするパーサ

正規表現の`*`と同じです

### (<|>) :: Parser a -> Parser a -> Parser a ###

一つ目のパーサが失敗時に二つ目のパーサを実行するパーサ

これは以下のように使います。

```haskell
parseTest (string "hello" <|> string "world") "world" -- パース成功
```

`Text.Parsec`はちょっと癖があって、
二つ目のパーサが実行されるのは
一つ目のパーサが __入力を全く消費せずに__ 失敗した場合だけになります。
たとえば

```haskell
parseTest (string "hello" <|> string "heaven") "heaven" -- パース失敗
```
のような場合、前段のhelloパーサが`he`まで消費してしまい、
heavenパーサが実行されません。

こういう場合はtryという関数も用意されているので併用してください。

```haskell
parseTest (try (string "hello") <|> string "heaven") "heaven" -- パース成功
```

### choice :: [Parser a] -> Parser a

リスト中のどれかが成功するまで解析を行うパーサ


Parser for List Tag &mdash; リストタグの為のパーサ
-------------------------------------------------

ListP.hsには冒頭のタグを解析するパーサを自前実装しています。

Parser Combinatorの動作の仕組みや、使い方を知りたい人は
是非眺めてみてください。

