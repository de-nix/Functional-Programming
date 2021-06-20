module GoneWild(func) where
import Data.Monoid
sum :: (Foldable t, Num a) => t a -> a
sum = foldMap Sum
product :: (Foldable t, Num a) => t a -> a
product = foldMap Product
elem :: (Foldable t, Eq a)
  => a -> t a -> Bool
elem a ta = foldMap any . (==a) ta

newMin :: Ord x =>x-> Maybe x-> Maybe x
newMin a Nothing = Just a
newMin a (Just b) = Just (min a b)

newMax :: Ord x => x-> Maybe x -> Maybe x

newMax a Nothing = Just a
newMax a (Just b) = Just (max a b)

minimum :: (Foldable t, Ord a)
  => t a -> Maybe a
minimum = foldr newMin Nothing
maximum :: (Foldable t, Ord a)
  => t a -> Maybe a
maximum = foldr newMax Nothing



null :: (Foldable t) => t a -> Bool
null ta = foldr newNull True where
  newNull _ _ = False
length :: (Foldable t) => t a -> Int
length = foldr newLen 0 where
  newLen a b = 1 + b
--8. Some say this is all Foldable amounts to.
toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []
--9. Hint: use foldMap.
-- | Combine the elements
-- of a structure using a monoid.
fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (<> mempty)
--10. Defne foldMap in terms of foldr.
gfoldMap :: (Foldable t, Monoid m)
  => (a -> m) -> t a -> m
gfoldMap g = foldr (mappend . g) mempty
--20.6 Chapter Exercises
--Write Foldable instances for the following datatypes.
data Constant a b =
  Constant b deriving (Eq, Show)
instance Foldable (Constant a b) where
  foldr f x (Constant a) = f a x
data Two a b =
  Two a b
instance Foldable (Two a b) where
  foldr f b' (Two a b) = f b b'
data Three a b c =
  Three a b c
instance Foldable (Three a b c) where
  foldr f c' (Three a b c) = f c c'
data Three' a b =
  Three' a b b

instance Foldable (Three a b b) where
  foldr f c' (Three a b c) = f b c' `mappend` f c c'
data Four' a b =
  Four' a b b b
--Thinking cap time. Write a flter function for Foldable types using
--foldMap.


tf f x = if f x then pure x else mempty

filterF :: ( Applicative f, Foldable t, Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF f = foldMap (tf f)
