=============================
haddock,doctest導入
=============================

### Morita Yasuaki
#### Feb 20, 2013

テストやドキュメンテーションに
**doctest**が便利なので紹介します。

cabal、doctestのインストール設定等
---------------------------------

まず **cabal** を新しくします。
cabalはghc向けのパッケージ管理ソフトです。
結構融通の効かないやつです。

+ パッケージのアンインストール機能がない
+ 変なキャッシュやログが残ると手で処理してあげないといけない
+ パッケージ依存関係が一回おかしくなるとヤバい


まずはこの **cabal** やっつけます。

haskell-platformは入ってるとして、

```bash
$ cabal install cabal-install
```

とshellに打ちましょう。

cabalにcabal-installをinstallさせるという意味不明な状態ですが、
haskell-platformに元から入ってるcabalで新しめのcabalをインストールするという意味です。  
なのでPCにはcabalが二つ入ります。  
cabal-devというプロジェクト毎に
パッケージを管理するやつもあるので好みで使ってください。

自分の場合、時間もかかるし、容量も食うので、cabalでどっからでも使えるようにしちゃってます。  
(地雷っぽいパッケージはcabal-devで入れます)

インストールが終わったら

```bash
$ which cabal
```


でcabalの所在地を調べましょう。  
もしホームディレクトリ以下($HOME/.cabal/bin/cabal)
でなければ、古いcabalが使われているままです。  
PATHをゴニョゴニョして、新しいcabalが優先して使われるようにしておきます。

```bash
$ export PATH=$HOME/.cabal/bin:$PATH
```


で、早速doctestをインストール。。。する前に、  
一応

```bash
$ cabal update
```

でパッケージ情報を最新にしておきます。


あと、cabal初回起動時に**$HOME/.cabal/config**というファイルが作られるのですが、
自分好みに中身を書き換えた方がいいかも知れません。
僕はなんとなくdocumentation=Trueにしています。
(デフォルトドでキュメント付きでパッケージインストールするの意味)

```bash
$ cabal install haddock doctest doctest-prop
```

とやればインストールできる。。。はず（覚えてない）。  
すんなりいかなくてもcabalの吐くメッセージとか読んで、
依存パッケージのインストールとかやっていけば大丈夫だと思います。


cabalで何も考えずにインストールしたパッケージは、
さっきPATHを通した$HOME/.cabal/binに配置されるので
すぐ使えると思います。

開発環境整える系では

+ ghc-mod
+ HLint

あたりを追加で入れとくといいかも知れません。
僕はlintはあんまり使ってないですが、
ghc-modにはお世話になっています。

doctestとhaddock の使い方
-------------------------

**doctest** というのはpythonにある開発用のツールで
コメントに、書式に従ってテストケースを書いとくと
テスト実行ができて、
なおかつドキュメンテーションとしても残せる
という仕組みのやつです。
で、これのghc版があるのでこれを使います。


+ コメントの最初に`|`を書くと、その下にある関数のドキュメンテーションになる
+ コメントの最初に`^`を書くと、その上にある関数のドキュメンテーションになる
+ `>>>`を書くとghciでの実行、次の行が予期される結果を意味します(doctestでユニットテストできます)
+ `prop>`を書くと性質(任意の整数に対して成り立つみたいなの)を表します(doctestでQuick Checkが走ります)

`>>>`の方はghciで実行した結果と文字列比較するだけのものなので
例外でもテストできます。


ドキュメントとテストケース書き方はだいたいこんな感じで、
見てもらえば分かると思います。

> double.hs

```haskell
{- |
Module :   Example
Description :   doctest-haddock-example
Copyright :   (c) Morita Yasuaki
License :   BSD liscense
-}

module Example where

-- |
-- doubleのテストケースだよ
-- >>> double 4
-- 8

double :: Int -> Int
double a = 2*a

-- ^
-- doubleの他の利用例
-- >>> double 12
-- 24
-- double関数の性質
-- prop> double x `div` 2 = x
```

テストしたい時は

```bash
$ doctest *.hs
```

とかやるとテストケースの実行ができます。

```bash
$ haddock --html *.hs -o doc
```

でdoc以下にドキュメントが生成されます。
この辺はhelpを読むなりして使いこなしてください。

感想
----

脅威のVimテクノロジの一つ、Quick Runプラグインを使い、  
Vim上2キーストロークでdoctestが走るようにしています。  
演習問題を解く時に非常に役に立っています。  
あんまりghciを起動しなくなりました。

> ~/.vimrc

```vim
let g:quickrun_config['haskell'] = {
\ 'command': 'doctest'
\ }
```

メリットとしては

+ コンパイルが通らない時はコンパイルエラーが出力される
+ 書式が単純でかなり気軽にテストケースが書ける
+ ちょっとした実験にも使える(エディタとghciを行ったり来たりせずに)


手探りの実験と実装とテストの間を行来するハードルが非常に低いので、  
とても便利、快適です。

さらに、こうしておけばhaddockで
ドキュメンテーションもできるというおまけ付きです。  
(ただ日本語は消されます。悲しい。)