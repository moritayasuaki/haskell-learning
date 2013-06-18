Parsec 入門 
===========


Recursive Decent Parsing &mdash; 再帰下降構文解析
-------------------------------------------------

再帰下降構文解析とは...

再帰的な構文を上から舐めて行くパーサです。

以下のようなulとolとliの三つのタグからなるhtmlがあるとします

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
<ul contenteditable=true>
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

上のような文は以下のような再帰的な構文を持つものとしてパースすることができます。

`EBNF記法`

```
listheader = "<ul>", listbody, "</ul>" | "<ol>", listbody, "</ol>" ;
listbody = { "<li>", listcontents, "</li>" } ;
listcontents = rawstring | listheader
```
上ではstringの定義を抜いていますが
ここではアルファベットからなる文字列とします。

またホワイトスペースのパース記述も省略しています。


Parser Combinator &mdash; パーサコンビネータ
--------------------------------------------

haskellのように関数がファーストクラスである言語と
再帰下降構文解析は非常に相性がいいです。
小さなパーサや高階関数の組み合わせで
いろいろなパーサが作れるからです。

パーサを「入力文字列を先頭から消費して、内容に応じて何らかの出力結果を返す関数」と考えます。

Haskellではパーサはどんな型になるか考えてみます。

`単純な実装`

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

```haskell
-- 引数のStringは入力文字列
-- aはパーサの出力の型
type Parser a = String -> (a, String)
```

こうしておくと前段のパーサが食い残した文字列を
後続のパーサ食わせるように連鎖させる事ができます。
実はこれは後で学ぶStateモナドと同じ形をしています。

もう一つ考える事があります。
それはパースの失敗です。
gccにhaskellのコードを食わせてもシンタックスエラーが起こるように、
規定の構文に従わない物は拒否する必要があります。
これは失敗の可能性表すMaybeで良さそうです。

Maybeでもいいのですが、
一応なぜシンタックスエラーが起きたかを後で知れるように
Eitherにしておきましょう。

```haskell
type Parser a = String -> Either String (a, String)
```

なんかややこしくなってきたので、
もうちょっと意味が分かりやすい型シグネチャにしてみましょうか。

単に名前を付け替えただけで本質的には何も変わっていません。

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
もう少し高機能&汎用的なので、ややこしいシンタックスになっていますが、
基本は同じです。

Combinator $mdash 代表的なパーサコンビネータ
-------------------------------------------

原理は分かりましたが、
大きなパーサを組み上げるためには、
もう少し部品となるパーサを用意しておくと便利です。
`Text.Parsec`にも標準で入っている
代表的な部品を挙げます。

### char :: Char -> Parser Char ###

特定の文字のみ受け付けるパーサを返す関数

例えば

```haskell
parseTest (char 'h') "h" -- パース成功
parseTest (char 'h') "a" -- パース失敗
```

のように動作します

### string :: String -> Parser String ###

特定の文字列のみ受け付けるパーサを返す関数

例えば

```haskell
parseTest (char 'h') "h" -- パース成功
parseTest (char 'h') "a" -- パース失敗
```

のように動作します

### many :: Parser a -> Parser [a] ###

パーサを取って、繰り返しをするパーサを返す関数

正規表現の`*`と同じです

### (<|>) :: Parser a -> Parser a -> Parser a ###

一つ目のパーサが失敗したら二つ目のパーサを実行するパーサを作る関数

Parsecはちょっと癖があって、
二つ目のパーサが実行されるのは
一つ目のパーサが __入力を全く消費せずに__ 失敗した場合という点です。
tryという関数も用意されているので併用してください。

### choice :: [Parser a] -> Parser a

引数のリストのいずれかが成功するまでトライするパーサを返す関数


Paser for List Tag $mdash; リストタグの為のパーサ
-------------------------------------------------

ListP.hsには冒頭のタグを解析するパーサを自前実装しています。
Parser Combinatorの動作の仕組みや、使い方を知りたい人は
是非見てみてください。

