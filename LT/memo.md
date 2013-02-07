~~~~

ghci> head []
*** Exception: Prelude.head: empty list

~~~~

部分関数の例外です。
だいたいパターンマッチのマッチ先が見つからない時に出てくると思います。

~~~~

Parse error

~~~~

括弧付けのミスか行揃えのミスか、
もしくは型アノーテーション(::)の付け忘れかも
僕は

~~~~

test x:xs = ...
-- test (x:xs) = ...

~~~~

みたいに、パターンマッチ部に括弧忘れてる場合が多いです。

~~~~

No instance for ...
Possible fix: add an instance declaration for ...

~~~~

print,show,(==)のように型クラス制約のある関数
を呼び出したときに表示されます。
ghciだと暗黙でprintが呼ばれます。
Showクラスの型じゃないのはprintできないのでよく出てきます。
（関数はprintできないとか）

インスタンス宣言追加しろ、と言われていますが、
だいたい引数の型が間違っている場合が多いです。

データ型を自作している場合は、本当にインスタンス宣言忘れをしている事が多いです。

~~~~

The function `Just' is applied to two arguments,
but its type `a0 -> Maybe a0' has only one

~~~~

適用がの回数がヘンなばあい。
1変数関数に２回値を適用してるとか。
これは結構分かりやすい場合が多いです。


~~~~
Couldn't match expected type `String' with actual type `Char'
  Expected type: String -> String
  Actual type: [Char] -> Char
~~~~

型が合ってないパターン。
これもよく出くわします。
使っている関数の型がどうなってるか、
型アノーテーションが合っているかどうかチェックしましょう。
Expectedの方は型アノーテーション、
Actualの方が型推論によるものです。
適当にプログラミングしてる時に出くわして
適当に修正しようとするとさらに頭がこんがらがります。

~~~~
Ambiguous type variable `a0' in the constraint:
  (Read a0) arising from a use of `read'
Probable fix: add a type signature that fixes these
  type variable(s)
~~~~

これもたまに出ますが、型が一意に決まらない場合にでます。
readなどは返り値の型が引数からは一意に決まりません。

~~~~
read :: (Read a) => String -> a
~~~~

こういう場合は型アノーテーションを追加するか、
しょっちゅうこのエラーが出てくるようなら、
多相性のない関数を自分で再定義してやるのもいいかも

~~~~
readInt :: String -> Int
readInt = read
~~~~




~~~~
  Not in scope: `sort'
  Perhaps you meant `sqrt` (imported from Prelude)
~~~~

識別子がスコープにないといっています。
スペルミスが無いなら、import忘れが無いか、
宣言ミスが無いか見直す。

~~~~
Ambiguous occurrence `foldr'
    It could refer to either
      ...
~~~~

識別子から定義が一意に決まらないと言っています。
モジュールをimportしまくってると結構起こります。
Preludeの基本関数の拡張版、特殊版が同じ識別子で別のライブラリで定義されていたり、
結構初心者泣かせなところがあるので注意。


+ Data.Foldable
+ Data.ByteString
+ Control.Applicative
+ Data.Set
+ Data.Text.Persec

~~~~
The type signature for `...' lacks an accompanying binding
~~~~

型宣言だけあって、定義が無いパターンです。
スペルミスとか実装忘れが無いか確認してください。
