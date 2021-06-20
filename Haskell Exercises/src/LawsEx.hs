module LawsEx where

import Laws
import Data.Monoid
import Test.QuickCheck
data Optional a = Nada | Only a deriving(Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> (Only b) = Only (a<>b)
  Only a <> Nada = Only a
  Nada <> Nada = Nada

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b)=Only (mappend a b)

newtype First' a = First' { getFirst' :: Optional a }deriving (Eq, Show)

instance Semigroup (First' a) where
 (<>) (First' Nada) (First' b) = (First' b)
 (<>) (First' a) (First' b) = (First' a)


instance Monoid (First' a) where
   mempty = (First' Nada)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    q <- arbitrary
    frequency [(1,return (First' (Only q))),(1,return (First' Nada))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend
type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool
main :: IO ()
main = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)
    putStrLn "srviue"
