module Checkers where
import           Test.QuickCheck
import           Test.QuickCheck.All
import Control.Monad(join)
data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)
--Remember what you wrote for the list Functor:
instance Functor List where
  fmap _ Nil= Nil
  fmap f (Cons a b)= Cons (f a) (fmap f b)
--Writing the list Applicative is similar.
instance Semigroup (List a) where
  (<>) Nil b = b
  (<>) b Nil = b
  (<>) (Cons a b) x = Cons a (b <> x)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _= Nil
  (<*>) _ Nil= Nil
  (<*>) (Cons a' b') b= fmap a' b <> (b' <*> b)

--join :: Monad m => m (m a) -> m a
--fmap :: Functor f => (a->b) -> f a -> f b
bind :: Monad m => (a -> m b) -> m a -> m b
bind f ma= join (fmap f ma)