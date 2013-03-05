% 飲みLT
% Morita Yasuaki
% Feb 1, 2013

Introduction
============

飲み会までの時間スタバでドヤりながら
今日のLTの内容について考えてました

(1) pandocの紹介
(2) GHCのコンパイルエラーとその対策
(3) 書籍「関数型プログラミングの楽しみ」の紹介

みんなが興味ありそうな内容を話したいと思います。


pandocの紹介
===========

pandocはmarkdown,html,pdf
などなど、色々な文書形式を相互変換できる
便利なhaskell製プログラムです。
このHTMLもpandocでつくっています。

~~~~
cabal install pandoc
~~~~

で多分インストールできると思います。

~~~~
pandoc hoge.md
~~~~

でmarkdownの変換先のhtmlが標準出力に出力されます。

~~~~
pandoc -s hoge.md -o hoge.html
~~~~

で単品で表示できるhtmlページを作れます。

~~~~
pandoc --toc -s hoge.md -o hoge.html
~~~~

で目次付きで出力してくれます。


~~~~
pandoc --toc -s -c pandoc.css hoge.md -o hoge.html
~~~~

cssで整形したhtmlを作れます。


ほかにもなんかpdfにできたり、いろいろできるらしい。


GHCコンパイルエラーとその対策
============================

Exception
=========

~~~~
ghci> head []
*** Exception: Prelude.head: empty list
~~~~

実行時例外です。

だいたいパターンマッチのマッチ先が見つからない時に出てくると思います。

Parse error
===========

括弧のミス、where,caseの後の行揃えのミスが多いです。

~~~~
myfunc x = case x of
             1 -> "one"
              2 -> "two"

parse error on input `2'
~~~~

とか、

~~~~
myhead x:xs = x

Parse error in pattern: myhead
~~~~

です。

僕はパターンマッチ部に括弧を忘れる事が多いです。

No instance
===========

~~~~
ghci> head

No instance for (Show ([a0] -> a0))
  arising from a use of `print'
Possible fix: add an instance declaration for ...
~~~~

主にprint,show,(==)のような、
型クラス制約のある関数を呼び出したときに表示されます。

特にghciだと暗黙でprintが呼ばれます。

Showクラスの型じゃない物を書いて、よく怒られます。
（関数はprintできないとか）

インスタンス宣言追加すれば直るよ、と言われてますが、
たいてい見当違いの場合が多いです。

データ型を自作している場合は、本当にインスタンス宣言忘れをしている事が多いです。たとえば

~~~~
data MyList a = Cons a (MyList a) | Nil
        deriving (Show,Eq)
~~~~

ここでShowを忘れるとMyListはprintできません。

また、Eqを忘れると==で比較ができません。

Many arguments
==============

~~~~
ghci> Just 1 2

The function `Just' is applied to two arguments,
but its type `a0 -> Maybe a0' has only one
~~~~

関数適用の回数がヘンな場合です。

1変数関数に２つ引数を適用してるとか。

これが出ると間違いが分かりやすいのですが、
似たようなミスでも型クラス制約が絡むとNo instanceとか他のメッセージに
変化する事が多いです。

~~~~
mysucc = succ 1 1

No instance for ...
~~~~

Couldn't match expected type
============================

~~~~
hello = "hell" ++ 'o'

Couldn't match expected type `[Char]' with actual type `Char'
~~~~

型推論と型アノーテーションが合わないパターン。

これもよく出くわします。

型がよくわかんないままプログラミングしてる時に出くわします。

適当に修正しようとするとさらに頭がこんがらがります。

使っている関数の型や型アノーテーションの勘違い、見落としに注意。

Ambiguous type variable
=======================

~~~~
read "1.5"

Ambiguous type variable `a0' in the constraint:
  (Read a0) arising from a use of `read'
Probable fix: add a type signature that fixes these
  type variable(s)
~~~~

これもたまに出ますが、型が一意に決まらない場合にでます。

readなどは返り値の型が引数だけでは決められません。

~~~~
read :: (Read a) => String -> a
~~~~

こういう場合は型アノーテーションを追加するか、
しょっちゅうこのエラーが出てくるようなら、
多相じゃない関数を自分で定義して、そっちを使うのもいいかも

~~~~
readInt :: String -> Int
readInt = read
~~~~


Not in scope
============

~~~~
sort [2013,2,1]

Not in scope: `sort'
Perhaps you meant `sqrt` (imported from Prelude)
~~~~

識別子がスコープにないといっています。

スペルミスの疑いは結構親切に教えてくれます。

スペルミスが無いなら、import忘れが無いか、
宣言ミスが無いか見直します。

Ambiguous occurrence
====================

~~~~
import Data.Foldable
sum' = foldr (+) 0

Ambiguous occurrence `foldr'
It could refer to either
  ...
~~~~

識別子から定義が一意に決まらないと言っています。

モジュールをimportしまくってると結構起こります。

Preludeの基本関数の拡張版、特殊版が同じ識別子で別のライブラリで定義されていたり、
結構初心者泣かせなところがあるので注意。



Lacks an accompanying binding
=============================

~~~~
paren :: String -> String
paren' a = "(" ++ a ++ ")" 

The type signature for `paren' lacks an accompanying binding
~~~~

型宣言だけあって、定義が無いパターンです。

スペルミスとか実装忘れが無いか確認してください。

Rigit type variable bound
=========================

~~~~
Couldn't match type `edgeType' with `Edge'
  `edgeType' is a rigid type variable bound by
             the type signature for
               connect :: Graph edgeType -> Node -> Graph edgeType
~~~~

型制約の違反に関するエラー。
制約付きのポリモーフィズムを使ってる場合にたまに起こる。
ゆるい型制約の元で使える変数を
型制約の厳しい関数に渡したとき等に起こる？
型クラスの記述や、関数同士の関係を良く見て、
原因を突き止める必要がある。
（簡単な解決方法は僕もよく分かってない）


その他のエラー
==============

自分でクラスを作る場合、インスタンス宣言をする場合、
かなり難しそうなエラーに出くわします。

このへんはGHC拡張が前提になったり、
詳しく説明できる知識もないので割愛。
