module MonoidEx (func) where

import Test.QuickCheck
import Data.Monoid
import Laws(semigroupAssoc, monoidLeftIdentity, monoidRightIdentity, monoidAssoc)

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial
instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)
instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivAssoc =
    Trivial -> Trivial -> Trivial -> Bool

func :: IO ()
func = do
  let mli = monoidLeftIdentity
      mlr = monoidRightIdentity
      rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  quickCheck (mlr :: Two (Product Int) (Sum Int) -> Bool)
  quickCheck (mli :: Two (Product Int) (Sum Int)-> Bool)
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0
  print $ fmap (10/) (4, 5, 100)

newtype Mem s a =Mem {
  runMem :: s -> (a,s)
  }
instance Semigroup a => Semigroup (Mem s a) where
  (<>) (Mem f) (Mem f') = Mem (\n -> (fst (f n) <> fst (f' n), snd $ f $ snd $ f' n ))
instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \n ->(mempty,n)
  mappend = (<>)
  
f' = Mem $ \s -> ("hi", s + 1)
--A correct Monoid for Mem should, given the above code, get the
--following output:
--Prelude> main
--("hi",1)("hi",1)("",0)TrueTrue
--Make certain your instance has output like the above, this is
--sanity-checking the Monoid identity laws for you! It’s not a proof
--and it’s not even as good as property testing, but it’ll catch the
--most common mistakes people make.
--It’s not a trick and you don’t need a Monoid for � Yes, such a
--Monoid can and does exist. Hint: chain the �values from one
--function to the other. You’ll want to check the identity laws as a
--common frst attempt will break them




newtype Identity a =
  Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mappend (Identity a) (Identity b) = Identity (mappend a b)
  mempty = Identity mempty
instance Arbitrary a => Arbitrary (Identity a) where
   arbitrary = do
        y <- arbitrary
        return $ Identity y


data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b ) <> (Two c d) = Two (a <> c) (b <> d)
instance (Monoid a, Monoid b) => Monoid (Two a b) where 
  mempty = Two mempty mempty
  mappend = (<>)
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do x <- arbitrary 
                 Two x <$> arbitrary


newtype BoolConj = BoolConj Bool deriving (Show, Eq)

instance Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False
  
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)
  
instance Arbitrary BoolConj where
  arbitrary = frequency [(1, return (BoolConj True)),(1,return (BoolConj False))] 



--What it should do:
--Prelude> (BoolConj True) `mappend` mempty
--BoolConj True
--Prelude> mempty `mappend` (BoolConj False)
--BoolConj FalseCHAPTER 15. MONOID, SEMIGROUP 611
newtype BoolDisj = BoolDisj Bool
--What it should do:
--Prelude> (BoolDisj True) `mappend` mempty
--BoolDisj True
--Prelude> mempty `mappend` (BoolDisj False)
--BoolDisj False
newtype Combine a b =
  Combine { unCombine :: (a -> b) }
--What it should do:
--Prelude> f = Combine $ \n -> Sum (n + 1)
--Prelude> unCombine (mappend f mempty) $ 1
--Sum {getSum = 2}
--7. Hint: We can do something that seems a little more specifc and
--natural to functions now that the input and output types are the
--same.
newtype Comp a =
  Comp (a -> a)
--8. This next exercise will involve doing something that will feel a
--bit unnatural still and you may fnd it difcult. If you get it and
--you haven’t done much FP or Haskell before, get yourself a nice
--beverage. We’re going to toss you the instance declaration so
--you don’t churn on a missing Monoid constraint you didn’t know
--you needed.
--f' = Mem $ \s -> ("hi", s + 1)
--main = do
--let rmzero = runMem mempty 0
--rmleft = runMem (f' <> mempty) 0
--rmright = runMem (mempty <> f') 0
--print $ rmleft
--print $ rmright
--print $ (rmzero :: (String, Int))
--print $ rmleft == runMem f' 0
--print $ rmright == runMem f' 0
--A correct Monoid for Mem should, given the above code, get the
--following output:
--Prelude> main
--("hi",1)
--("hi",1)
--("",0)
--True
--True
--Make certain your instance has output like the above, this is
--sanity-checking the Monoid identity laws for you! It’s not a proof
--and it’s not even as good as property testing, but it’ll catch the
--most common mistakes people make.
--It’s not a trick and you don’t need a Monoid for � Yes, such a
--Monoid can and does exist. Hint: chain the �values from one
--function to the other. You’ll want to check the identity laws as a
--common frst attempt will break them
