module Reader where

import Data.Char
cap :: [Char] -> [Char]
cap = map toUpper
rev :: [Char] -> [Char]
rev = reverse
--Two simple functions with the same type, taking the same type of
--input. We could compose them, using (.) or fmap:
composed :: [Char] -> [Char]
composed = cap . rev
fmapped :: [Char] -> [Char]
fmapped = cap <$> rev
--The output of those two should be identical: one string that is
--made all uppercase and reversed, like this:
--Prelude> composed "Julie"
--"EILUJ"
--Prelude> fmapped "Chris"
--"SIRHC
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev
tupled' :: [Char] -> ([Char], [Char])
tupled' = do 
  a <- cap
  b <- rev
  return (a,b)
tupled2' :: [Char] -> ([Char], [Char])
tupled2' =  
  cap >>= (\x -> rev >>= (\y -> return (x,y)))

newtype HumanName =
  HumanName String
  deriving (Eq, Show)
newtype DogName =
  DogName String
  deriving (Eq, Show)
newtype Address =
  Address String
  deriving (Eq, Show)
data Person =
  Person {
    humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)



-- you'll need this pragma
{-# LANGUAGE InstanceSigs #-}
instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ \r -> a
  (<*>) :: Reader r (a -> b)
    -> Reader r a
    -> Reader r b
  (Reader rab) <*> (Reader ra) =
    Reader $ \r -> rab r (ra r)
instance Monad (Reader r) where
  return = pure
--CHAPTER 22. FUNCTIONS WAITING FOR INPUT 865
  (>>=) :: Reader r a
    -> (a -> Reader r b)
    -> Reader r b
  (Reader ra) >>= aRb =
      Reader $ \r -> runReader (aRb (ra r)) r