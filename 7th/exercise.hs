-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Exercise where


import Control.Applicative
import Control.Monad.Trans
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

-- |
-- EX1: Nullable can be Effective or Null
-- 
-- >>> :t Effective 100
-- Effective 100 :: Num a => Nullable a
-- >>> :t Null
-- Null :: Nullable a
-- >>> Effective 10 == Effective 10
-- True

data Nullable a = Effective a
                | Null
                deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Nullable a) where
    arbitrary = frequency [(1, return Null),(5, Effective <$> arbitrary)]

    shrink (Effective x) = Null : [ Effective x' | x' <- shrink x ]
    shrink _ = []


-- |
-- EX2: like HasValue, Value properties
--
-- >>> hasValue (Effective 100)
-- True
-- >>> hasValue Null 
-- False
-- >>> value (Effective 20)
-- 20
-- >>> value (Effective [1,2,3])
-- [1,2,3]
-- >>> value Null
-- *** Exception: invalid operation


hasValue :: Nullable a -> Bool
hasValue (Effective _) = True
hasValue Null          = False

value :: Nullable a -> a
value (Effective v) = v
value Null          = error "invalid operation"


-- |
-- Ex3: divNull
--
-- prop> divNull 1 0 == Null
-- prop> divNull 2 1 == Effective 2
-- prop> divNull 0 1 == Effective 0
-- prop> divNull 0 0 == Null

divNull :: Integral a => a -> a -> Nullable a
divNull n 0 = Null
divNull n d = Effective (n `div` d)


-- |
-- EX4: divNullEx
--
-- prop> divNullEx Null Null           == Null
-- prop> divNullEx (Effective 20) Null == Null
-- prop> divNullEx Null (Effective 5)  == Null
-- prop> divNullEx (Effective 20) (Effective 0) == Null
-- prop> divNullEx (Effective 0) (Effective 10) == Effective 0
-- prop> divNullEx (Effective 0) (Effective 0) == Null
-- prop> divNullEx (Effective 100) (Effective 20) == Effective 5

divNullEx :: Integral a => Nullable a -> Nullable a -> Nullable a
divNullEx (Effective n) (Effective d) = divNull n d
divNullEx _ _ = Null

-- |
-- divNull by do notation
--
-- prop> divNullEx' x y == divNullEx x y
--                 where types = (x,y) :: (Nullable Int, Nullable Int)

divNullEx' :: (Integral a) => Nullable a -> Nullable a -> Nullable a
divNullEx' nn nd = do
    n <- nn
    d <- nd
    divNull n d


-- |
-- EX5: (b) doubleNull
--
-- prop> doubleNull (Effective 100)   == Effective 200
-- prop> doubleNull (Effective 200.1) == Effective 400.2
-- prop> doubleNull Null            == Null

doubleNull :: Num a => Nullable a -> Nullable a
doubleNull (Effective x)= Effective (2*x)
doubleNull Null = Null


-- |
-- EX5: (a) instance declarations of Nullable


-- |
-- prop> doubleNull x == fmap (*2) x
--                  where types = x :: Nullable Int

instance Functor Nullable where
    fmap f (Effective x) = Effective (f x)
    fmap f Null          = Null

-- |
-- >>> mod <$> Effective 10 <*> Effective 3
-- Effective 1

instance Applicative Nullable where
    pure x  = Effective x
    Effective x <*> Effective y = Effective (x y)
    _ <*> _                       = Null

-- |
-- Max bound value of Int is dependent to enviroment
--
-- >>> divNull 10 0 <|> Effective maxBound :: Nullable Int
-- Effective 9223372036854775807

instance Alternative Nullable where
    empty = Null
    Effective x <|> _    = Effective x
    Null <|> Effective y = Effective y
    _ <|> _              = Null

-- |
-- >>> sequence (map (divNull 10) [1,0,2])
-- Null
-- >>> sequence (map (divNull 10) [1,2,5])
-- Effective [10,5,2]

instance Monad Nullable where
    fail _ = Null
    return = pure
    Effective x >>= f = f x
    _ >>= _           = Null

-- |
-- >>> runNullableT $ do { a <- lift [1..2]; return (a*2) }
-- [Effective 2,Effective 4]

-- NullableT :: m (Nullable a) -> NullableT m a
-- runNullableT :: NullableT m a -> m (Nullable a)
newtype NullableT m a = NullableT { runNullableT :: m (Nullable a) }

instance (Functor m) => Functor (NullableT m) where
    -- x                :: NullableT m a
    -- runNullableT $ x :: m (Nullable a)
    -- f                :: a -> b
    -- fmap f           :: Nullable a -> Nullable b
    -- fmap (fmap f)    :: m (Nullable a) -> m (Nullable b)
    -- fmap (fmap f) . runNullableT $ x             :: m (Nullable b)
    -- NullableT . fmap (fmap f) . runNullableT $ x :: NullableT m b
    fmap f = NullableT . fmap (fmap f) . runNullableT

instance (Monad m) => Monad (NullableT m) where
    fail _ = NullableT $ return Null
    -- x                :: a
    -- return x         :: Nullable a
    -- return . return $ x :: m (Nullable a)
    -- NullableT . return . return $ x :: NullableT m a
    return = NullableT . return . return

    -- x                    :: NullableT m a
    -- runNullableT x       :: m (Nullable a)
    -- (runNullableT x >>=) :: (Nullable a -> m (Nullable b)) -> m (Nullable b)
    -- f                    :: a -> NullableT m b
    -- x'                   :: Nullable a
    -- runNullableT . f     :: a -> m (Nullable b)
    -- f'                   :: Nullable a -> m (Nullable b)
    x >>= f = NullableT (runNullableT x >>= f')
      where 
        f' Null          = return Null
        f' (Effective x') = runNullableT . f $ x'

instance MonadTrans NullableT where
    -- x :: m a
    -- NullableT :: m (Nullable a) -> NullableT m a
    -- (x >>=)   :: (a -> m (Nullable b)) -> m (Nullable b)
    -- lift :: m a -> Nullable m a
    lift x = NullableT (x >>= return . return)


