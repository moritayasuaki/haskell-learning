# Monad Transformer
## Morita Yasuaki
## Aug 1, 2013

少し大きいプログラムになると、
いろいろな文脈が必要になる。

異なるモナドを組み合わせたり、行き来する統一的な方法は無いか？

haskellでの解決策 => Monad Transformer

モナドを取ってモナドを返す型レベルの関数

## モナド変換子の位置づけ

> 型

```haskell
Int, String, Bool :: *

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
```

> 型コンストラクタ

```haskell
Maybe, [], IO :: * -> *

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
```

> 高階型コンストラクタ

```haskell
MaybeT, StateT, ErrorT :: (* -> *) -> (* -> *)

class MonadTrans t where
  lift :: m a -> (t m) a
```

## Maybeモナド変換子

```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

-- Just関数をmの世界に持ちあげて使っている
-- Just :: a -> Maybe a
-- liftM Just :: m a -> m (Maybe a)
-- MaybeT . liftM Just :: m a -> MaybeT m a

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

-- runMaybeT xでv :: Maybe aを取り出して
-- 失敗情報の伝搬/成功時の計算連鎖を定義している

instance (Monad m) => Monad (MaybeT m) where
   return = lift . return  
   x >>= f = MaybeT $ do
       v <- runMaybeT x
       case v of
           Nothing -> return Nothing
           Just y -> runMaybeT (f y)
```

## 利用法

### チェス盤上のナイトの経路を列挙する

```hasell 
-- WriterTとListTを組み合わせる
type Path = WriterT [Pos] (ListT Identity)
```

### パーサを作る

```hasell 
-- StateTとErrorTを組み合わせる
type Parser = StateT String (ErrorT ErrorMessage Identity)
```

## 厄介な点

- liftの多重呼び出しによるオーバーヘッド
- 変換子の組み合わせる順序で意味が変わる
- かなりややこしい
