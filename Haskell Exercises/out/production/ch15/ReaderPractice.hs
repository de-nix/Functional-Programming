module ReaderPractice where

import Control.Applicative
import Data.Maybe
x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]
lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y
-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z
--It’s also nice to have one that will return Nothing, like this one:
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y
-- now zip x and z using a
-- variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z
x1 :: Maybe (Integer, Integer)
x1 = Maybe (,) <*> xs <*> ys
x2 :: Maybe (Integer, Integer)
x2 = Maybe (,) <*> ys <*> zs
x3 :: Integer
  -> (Maybe Integer, Maybe Integer)
x3 x= (z' x, z' x)
--Your outputs from those should look like this:
--ReaderPractice x1
--Just (6,9)
--ReaderPractice> x2
--Nothing
--ReaderPractice> x3 3
--(Just 9,Just 9)
--Next, we’re going to make some helper functions. Let’s use uncurry
--to allow us to add the two values that are inside a tuple:
--uncurry :: (a -> b -> c) -> (a, b) -> c
--The frst argument is a function in this case, we want it to be
--addition. summed is uncurry with addition as the frst argument.
summed :: Num c => (c, c) -> c
summed x = fst x + snd x
--And now we’ll make a function similar to some we’ve seen before
--that lifs a boolean function over two partially applied functions:
bolt :: Integer -> Bool
-- use &&, >3, <8
bolt = (&&) <$> (>3) <*> <<8>
--Finally, we’ll be using fromMaybe in the main exercise, so let’s look at
--that:
fromMaybe :: a -> Maybe a -> a
--You give it a default value and a Maybe value. If the Maybe value is a
--Just a, it will return the �value. If the value is a Nothing, it returns the
--default value instead:CHAPTER 22. FUNCTIONS WAITING FOR INPUT 870
--ReaderPractice> fromMaybe 0 xs
--6
--ReaderPractice> fromMaybe 0 zs
--0
--Now we’ll cobble together a main, so that in one call we can execute
--several things at once.
func :: IO ()
func = do
  print $
  sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
--When you run this in GHCi, your results should look like this:
--ReaderPractice> main
--Just [3,2,1]
--[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
--Just [6,9]
--Just 15
--Nothing
--True
--[True,False,False]
--Next, we’re going to add one that combines sequenceA and Reader
--in a somewhat surprising way (add this to main):
print $ sequenceA [(>3), (<8), even] 7
--The type of sequenceA isCHAPTER 22. FUNCTIONS WAITING FOR INPUT 871
sequenceA :: (Applicative f, Traversable t)
=> t (f a) -> f (t a)
-- so in this:
sequenceA [(>3), (<8), even] 7
-- f ~ (->) a and t ~ []
--We have a Reader for the Applicative (functions) and a traversable
--for the list. Pretty handy. We’re going to call that function sequA for
--the purposes of the following exercises:
sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m
--And henceforth let
s' = summed <$> ((,) <$> xs <*> ys)
--be known as s'.
--OK, your turn. Within the main above, write the following (you
--can delete everything afer do now if you prefer – just remember to
--use print to be able to print the results of what you’re adding):
--1. fold the boolean conjunction operator over the list of results of
--sequA (applied to some value).

fld m = foldr and True (sequA m) 
seqA $ fromMaybe 0 s'
--2. apply sequA to s'; you’ll need fromMaybe.
--3. apply bolt to ys; you’ll need fromMaybe