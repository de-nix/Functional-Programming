module Monads (func) where

import Control.Monad (join,liftM2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--data Sum a b =
--  First a
--  | Second b
--  deriving (Eq, Show)
--instance Functor (Sum a) where
--  fmap f (First a) = First a
--  fmap f (Second b) = Second (f b)
--instance Applicative (Sum a) where
--  pure = Second
--  (<*>) (First a) _ = First a
--  (<*>) _ (First b) = First b
--  (<*>) (Second f) (Second b) = Second(f b)
--  
--instance Monad (Sum a) where
--  return = pure
--  (>>=) (First a) _ = First a 
--  (>>=) (Second b) f = f b
--  
func = do
         putStrLn "\n1. Nope"
         quickBatch $ functor     nope
         quickBatch $ applicative nope
         quickBatch $ monad       nope
         putStrLn "\n2. PhhhbbtttEither"
         quickBatch $ functor     peither
         quickBatch $ applicative peither
         quickBatch $ monad       peither
         putStrLn "\n3. Identity"
         quickBatch $ functor     identity
         quickBatch $ applicative identity
         quickBatch $ monad       identity
         putStrLn "\n4. List"
         quickBatch $ functor     list
         quickBatch $ monoid      list
         quickBatch $ applicative list
         quickBatch $ monad       list


type SSI = (String, String, Int)
nope = undefined :: Nope SSI
peither = undefined :: BahEither String SSI
identity = undefined :: Identity SSI
list = undefined :: List SSI


data Nope a =
  NopeDotJpg deriving (Eq, Show)
instance Functor Nope where
  fmap f NopeDotJpg = NopeDotJpg
instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg
instance Eq a => EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg
  
-- We're serious. Write it anyway.
data BahEither b a =
  PLeft a
  | PRight b  deriving (Eq, Show)
instance Functor (BahEither b) where
  fmap f (PLeft a) = PLeft (f a)
  fmap f (PRight a)= PRight a
instance Applicative (BahEither b) where
  pure = PLeft
  (<*>) (PRight a) _ = PRight a
  (<*>) _ (PRight a) = PRight a
  (<*>) (PLeft f) (PLeft x) = PLeft (f x)
instance Monad (BahEither b) where
  return = pure
  (>>=) (PRight a) _ = PRight a
  (>>=) (PLeft a) f = f a
instance (Eq b, Eq a) => EqProp (BahEither b a) where
  (=-=) = eq
instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    elements [ PLeft y
             , PRight x
             ]
--3 a Monad instance for Identity.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity x)= Identity (f x)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x) 
instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a
instance Arbitrary a=> Arbitrary(Identity a) where
  arbitrary =do
    x<- arbitrary
    return (Identity $ x)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq  
--4. This one should be easier than the Applicative instance was.
--Remember to use the Functor that Monad requires, then see where
--the chips fall.
data List a =
  Nil
  | Cons a (List a)  deriving (Eq, Show)
instance Functor List where 
  fmap f Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)
instance Semigroup (List a) where
  (<>) Nil a = a
  (<>) a Nil = a
  (<>) (Cons x y) a = Cons x (y <> a)
  
instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f b) a =mappend (fmap f a) (b <*> a)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil 
  (>>=) (Cons x xs) f = mappend (f x) (xs >>= f)
instance Arbitrary a => Arbitrary(List a) where
  arbitrary = do
    x<- arbitrary
    return (Cons x Nil) 
instance Eq a => EqProp (List a) where
  (=-=) = eq
j :: Monad m => m (m a) -> m a
j = join
l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap
l2 :: Monad m=> (a -> b -> c) -> m a -> m b -> m c
l2 = liftM2--fmap f ma <*> mb
a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (h:t) f = ((:) <$> f h) <*> meh t f
meh [] _ = return [] 
flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id