module Lib
    ( 
    someFunc
    ) where

import Data.Monoid
import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
   Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
   arbitrary = return Trivial


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool
type IdeAssoc = Identity Trivial -> Identity Trivial ->Identity Trivial -> Bool
type TwoAssoc = Two Trivial Trivial-> Two Trivial Trivial-> Two Trivial Trivial -> Bool
type ThreeAssoc = Three Trivial Trivial Trivial -> Three Trivial Trivial Trivial -> Three Trivial Trivial Trivial -> Bool
type DisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type ConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type OrAssoc = Or Int Int -> Or Int Int -> Or Int Int -> Bool
type FuncAssoc = Combine (Int -> Int) Int ->Combine (Int -> Int) Int-> Combine (Int -> Int) Int-> Bool
type ValidAssoc = Validation String Int -> Validation String Int-> Validation String Int-> Bool
someFunc = do
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdeAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (semigroupAssoc :: DisjAssoc)
    quickCheck (semigroupAssoc :: ConjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
--    quickCheck (semigroupAssoc :: FuncAssoc)
    quickCheck (semigroupAssoc :: ValidAssoc)

    let failure :: String -> Validation String Int
        failure = Failure'
        success :: Int-> Validation String Int
        success = Success'
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
someFunc :: IO ()


newtype Identity a = Identity a deriving (Eq, Show)


instance Semigroup a => Semigroup (Identity a) where
    Identity a <> Identity b = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
      b <- arbitrary
      return (Identity b)

--CHAPTER 15. MONOID, SEMIGROUP 607
data Two a b = Two a b deriving(Eq, Show)


instance (Semigroup a, Semigroup b )=> Semigroup (Two a b) where
    Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b )=> Arbitrary (Two a b) where
    arbitrary = do
      x <- arbitrary
      y <- arbitrary
      return (Two x y)
--Hint: Ask for another Semigroup instance.
data Three a b c = Three a b c deriving(Eq, Show)

instance Arbitrary BoolDisj where
  arbitrary = do
      x <- arbitrary
      return $ BoolDisj x

instance Arbitrary BoolConj where
  arbitrary = do
      x <- arbitrary
      return $ BoolConj x

instance (Semigroup a, Semigroup b, Semigroup c )=> Semigroup (Three a b c) where
    Three a b c <> Three a' b' c'= Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c )=> Arbitrary (Three a b c) where
    arbitrary = do
      y <- arbitrary
      x <- arbitrary
      z <- arbitrary
      return (Three x y z)

data Four a b c d = Four a b c d deriving(Eq, Show)
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

--What it should do:
--Prelude> (BoolConj True) <> (BoolConj True)
--BoolConj True
--Prelude> (BoolConj True) <> (BoolConj False)
--BoolConj False
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where

  BoolDisj False <> BoolDisj False = BoolDisj False
  _ <> _ = BoolDisj True

--What it should do:
--Prelude> (BoolDisj True) <> (BoolDisj True)
--BoolDisj True
--Prelude> (BoolDisj True) <> (BoolDisj False)
--BoolDisj True
data Or a b = Fst a| Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd b <> _ = Snd b
  _ <> Snd b = Snd b
  _ <> Fst a = Fst a

instance (Arbitrary a, Arbitrary b) =>Arbitrary (Or a b) where
  arbitrary = do
      a <- arbitrary
      b <- arbitrary
      frequency [(1,return (Fst a)),(1,return (Snd b))]


----The Semigroup for Or should have the following behavior. We
----can think of this as having a “sticky” Snd value where it’ll hold
----onto the frst Snd value when and if one is passed as an argument.
----This is similar to the First' Monoid you wrote earlier.
----Prelude> Fst 1 <> Snd 2
----Snd 2
----Prelude> Fst 1 <> Fst 2CHAPTER 15. MONOID, SEMIGROUP 608
----Fst 2
----Prelude> Snd 1 <> Fst 2
----Snd 1
----Prelude> Snd 1 <> Snd 2
----Snd 1
newtype Combine a b = Combine { unCombine :: (a -> b) }


instance (Semigroup b) => Semigroup (Combine a b) where
  (Combine f ) <> (Combine g) = Combine (\n -> f n <> g n)



instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

----What it should do:
----Prelude> f = Combine $ \n -> Sum (n + 1)
----Prelude> g = Combine $ \n -> Sum (n - 1)
----Prelude> unCombine (f <> g) $ 0
----Sum {getSum = 0}
----Prelude> unCombine (f <> g) $ 1
----Sum {getSum = 2}
----Prelude> unCombine (f <> f) $ 1
----Sum {getSum = 4}
----Prelude> unCombine (g <> f) $ 1
----Sum {getSum = 2}
----Hint: This function will eventually be applied to a single value
----of type � But you’ll have multiple functions that can produce a
----value of type � How do we combine multiple values so we have
----a single � This one will probably be tricky! Remember that the
----type of the value inside of Combine is that of a function. The type
----of functions should already have an Arbitrary instance that you
----can reuse for testing this instance.
--newtype Comp a = Comp { unComp :: (a -> a) }
--
--
----Hint: We can do something that seems a little more specifc and
----natural to functions now that the input and output types are the
----same.CHAPTER 15. MONOID, SEMIGROUP 609
------11. -- Look familiar?
data Validation a b = Failure' a | Success' b deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  Success' a <> _ = Success' a
  _ <> Success' a = Success' a
  Failure' a <> Failure' b = Failure' ( a<> b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency[(1, return $ Failure' x),(1, return $ Success' y) ]
------Given this code:
----ssFunc = do
----    let failure :: String -> Validation String Int
----    failure = Failure
----    success :: Int-> Validation String Int
----    success = Success
----    print $ success 1 <> failure "blah"
----    print $ failure "woot" <> failure "blah"
----    print $ success 1 <> success 2
----    print $ failure "woot" <> success 2
----You should get this output:
----Prelude> main
----Success 1
----Failure "wootblah"
----Success 1
----Success 2
