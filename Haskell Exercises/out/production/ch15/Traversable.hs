module Traversable where
{-# LANGUAGE FlexibleContexts #-}

import Test.QuickCheck
import Test.QuickCheck.Checkers
--Identity
--Write a Traversable instance for Identity.
newtype Identity a = Identity a
  deriving (Eq, Ord, Show)


instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) =  Identity (f x)
instance Monad Identity where
  return = pure
  (>>=) (Identity a) f= f a
instance Foldable Identity where
  foldMap f (Identity a) = f a
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a
--Constant
newtype Constant a b =
  Constant { getConstant :: a }
instance Traversable (Constant a) where
  traverse f (Constant a) = Pure (Constant a)
instance Foldable (Constant a) where
  foldMap _ (Constant a) = mempty
--Maybe
data Optional a =
  Nada
  | Yep a
instance Traversable Optional where
  traverse _ Nada = Pure Nada
  traverse f (Yep a) = Yep <$> f a
instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a
--List
data List a =
  Nil
  | Cons a (List a)
instance Functor List where
  _ <$> Nil = Nil
  f <$> (Cons a b) = Cons (f a) (f <$> b)
instance Applicative List where
  _ <*> Nil = Nil
  Nil <*> _ = Nil
  (Cons f b) <*> c = mappend (f <$> c) (b <*> c)
  pure x= Cons x Nil 
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a b) f= mappend (f a) (b >>= f)
instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons a b) = mappend (f a) (foldMap f b)
  foldr _ x Nil = x
  foldr f x (Cons a b) = mappend ( f a x ) (foldr f x b)
instance Traversable List where
  traverse f Nil = Pure Nil
  traverse f (Cons a) = Cons <$> f a



--  CHAPTER 21. TRAVERSABLE 840
--Three
data Three a b c =
  Three a b c
instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance Applicative (Three a b) where
  (Three x y z) <*> (Three a b c) = Three (mappend x a) (mappend y b) (z c)
instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

--Pair
data Pair a b =
  Pair a b
--Big
--When you have more than one value of type � you’ll want to use Monoid
--and Applicative for the Foldable and Traversable instances respectively.
data Big a b =
  Big a b b
--Bigger
--Same as for Big.
data Bigger a b =
  Bigger a b b b
--S
--This may be difcult. To make it easier, we’ll give you the constraints
--and QuickCheck instances:

data S n a = S (n a) a deriving (Eq, Show)
--CHAPTER 21. TRAVERSABLE 841
instance ( Functor n
  , Arbitrary (n a)
  , Arbitrary a )
  => Arbitrary (S n a) where
  arbitrary =
    S <$> arbitrary <*> arbitrary
instance ( Applicative n
  , Testable (n Property)
  ,  Eq a
  , Eq (n a)
  , EqProp a)
  => EqProp (S n a) where
  (=-=) = eq
instance Traversable n
  => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a


main =
  sample' (arbitrary :: Gen (S [] Int))
--Instances for Tree
--This might be hard. Write the following instances for Tree.
data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)
instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a)  = Leaf $ f a
  fmap f (Node a b c) = Node (f <$> a) (f b) (f <$> c)
--CHAPTER 21. TRAVERSABLE 842
-- foldMap is a bit easier
-- and looks more natural,
-- but you can do foldr too
-- for extra credit.
instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node a b c) = mappend (foldMap f a) (mappend (f b) (foldMap f c))
instance Traversable Tree where
  traverse _ Empty = Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node a b c) = Node <$> traverse f a <*> f b <*> traverse f c
--Hints:
--1. For foldMap, think Functor but with some Monoid thrown in.
--2. For traverse, think Functor but with some Functor3 thrown in.

