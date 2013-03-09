=============================
haddock,doctest導入
=============================

### Morita Yasuaki
#### Feb 20, 2013

テストやドキュメンテーションに
**doctest** が便利なので紹介します。

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
$ cabal update
$ cabal install cabal-install
```

とshellに打ちましょう。

cabalにcabal-installをinstallさせるという変なコマンドですが、  
haskell-platformに元から入ってるcabalで新しめのcabalをインストールするという意味になります。  
なのでPCにはcabalが二つ入ります。

cabal-devというプロジェクト毎に
パッケージを管理するやつもあるので好みで使ってください。

自分の場合、時間もかかるし容量も食うので、基本はcabalでどっからでも使えるようにし、  
地雷っぽいパッケージだけcabal-devで入れてます。

インストールが終わったら

```bash
$ which cabal
```

でcabalの所在地を調べてみてください。  
もしホームディレクトリ以下($HOME/.cabal/bin/cabal)でなく、  
haskell-platformディレクトリ以下だと
古いcabalが使われているままになっていると思います。  
PATHをゴニョゴニョして、新しいcabalが優先して使われるようにしておきます。

```bash
$ export PATH=$HOME/.cabal/bin:$PATH
```


で、早速doctestをインストール。。。する前に、  
もう一回

```bash
$ cabal update
```

して、
新しく入れた方のcabalのパッケージ情報を最新にしておきます。


あと、cabal初回起動時に **$HOME/.cabal/config** というファイルが作られるのですが、
自分好みに中身を書き換えた方がいいかも知れません。
僕はなんとなくdocumentation=Trueにしています。
(ドキュメント付きでパッケージインストールするの意味)

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


+ コメントの最初に`|`を書くと、そのコメントの下にある関数のドキュメンテーションになる
+ コメントの最初に`^`を書くと、そのコメントの上にある関数のドキュメンテーションになる
+ `>>>`はghciでの実行例、次の行が予期される結果を意味します(doctestでユニットテストできます)
+ `prop>`を書くと性質(任意の整数に対して成り立つみたいなの)を表します(doctestでQuick Checkが走ります)

`>>>`の方はghciで実行した結果と文字列比較するだけのものなので
例外もテストできます。

ドキュメントとテストケース書き方はだいたいこんな感じで、
見てもらえば分かると思います。

> DocExample.hs

```haskell
{- |
Module :   DocExample
Description :   doctest-haddock-example
Copyright :   Morita Yasuaki
License :   BSD liscense
-}

module DocExample where

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

-- ^
-- double関数の性質
-- prop> double x `div` 2 == x

```

テストしたい時はdoctestコマンドを使います。

```bash
$ doctest DocExample.hs
```

とやるとテストケースの実行ができます。
以下のような結果が出てくれば成功です。

> doctest実行結果

```
Example: 3  Tried: 3  Errors: 0  Failures: 0
```

doctestについてはこれで終わりです。
非常にシンプルにテストができます。


次はドキュメントの生成ですが

```bash
$ haddock --html *.hs -o doc
```

でdoc以下にドキュメントが生成されます。
haddockはオプションが非常に多く、
全部は把握していません。
いろいろ実験してみるとおもしろいかも。


感想
----

メリットは

+ コンパイルが通らない時はコンパイルエラーが出力される
+ ghciへの入力をそのまま書けばテストケースになる

という事です。

さらにhaddockでドキュメンテーションもできるというおまけ付きです。  
(ただ日本語は消されます。悲しい。)


僕は脅威のVimテクノロジの一つ、QuickRunプラグインを使い、  
Vim上で2キーストロークでdoctestが走るようにしています。  

> ~/.vimrc

```vim
let g:quickrun_config = {}
let g:quickrun_config['haskell'] = {
\ 'command': 'doctest'
\ }

nnoremap <sirent> <leader>r :QuickRun<CR>
```

Vim以外のエディタを使ってる方でもdoctestを起動するショートカットを入れておくとよいと思います。  
実装とテストを行き来するのが非常に楽になると思います。
