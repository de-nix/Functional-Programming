module Applicative where

import Data.List (elemIndex)
--pure
--(<$>)
---- or fmap
--(<*>)
added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]
z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]
tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]
y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]
max' :: Int -> Int -> Int
max' = max
maxed :: Maybe Int
maxed = max' <$> x <*> y'
xs = [1, 2, 3]
ys = [4, 5, 6]
xx :: Maybe Integer
xx = lookup 3 $ zip xs ys
yy :: Maybe Integer
yy = lookup 2 $ zip xs ys
summed :: Maybe Integer
summed = sum <$> ((,) <$> xx <*> yy)

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a)= Identity (f a)
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity a) = Identity (f a)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)
instance Functor (Constant a) where
  fmap f (Constant a) = Constant a
instance Monoid a
  => Applicative (Constant a) where
  pure _= Constant mempty
  (<*>) (Constant f) (Constant b)= Constant (f `mappend` b)
