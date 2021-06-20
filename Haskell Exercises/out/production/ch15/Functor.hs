{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Functor (func) where
import Test.QuickCheck

func = do
          let 
            f :: Identity Int-> Bool 
            f x = functorIdentity x
            g :: Two Int Bool -> Bool
            g x = functorIdentity x
            h :: Four Int Bool String Float -> Bool
            h x = functorIdentity x
            k :: Four' Int Char -> Bool
            k x = functorIdentity x
          quickCheck f
          quickCheck g
          quickCheck h
          quickCheck k
          print (const <$> Just "Hello" <*> Just "World")
          print ((,,,) <$> Just 90
            <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3])

--  print a
--  print b
--  print (c 1)
--  print (d 0)
--  e
--  
--  
--a = fmap (+1) $ read "[1]" :: [Int]
--
----Prelude> a
----[2]
--b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])
----Prelude> b
----Just ["Hi,lol","Hellolol"]
--c = (* (2::Int)) . (\x -> x - 2)
------Prelude> c 1
-------2
--d =(return '1' ++) . show . (\x -> [(x :: Int), 1..3])
----Prelude> d 0
----"1[0,1,2,3]"
--e :: IO Integer
--e = let ioi = readIO "1" :: IO Integer
--        changed =  fmap read (fmap ("123" ++) ( fmap show ioi)) in fmap (*3) changed
------Prelude> e
------3693
--
--
functorIdentity :: (Functor f, Eq (f a)) => f a-> Bool
functorIdentity f = fmap id f == f
functorCompose :: (Eq (f c), Functor f) =>(a -> b) -> (b -> c)-> f a -> Bool
functorCompose f g x =
  (fmap g (fmap f x)) == (fmap (g . f) x)


newtype Identity a = Identity a deriving (Eq, Show)
data Pair a = Pair a a deriving (Eq, Show)
data Two a b = Two a b deriving (Eq, Show)
data Three a b c = Three a b c deriving (Eq, Show)
data Three' a b = Three' a b b deriving (Eq, Show)
data Four a b c d = Four a b c d deriving (Eq, Show)
data Four' a b = Four' a a a b deriving (Eq, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance (Arbitrary a) =>  Arbitrary (Identity a) where
  arbitrary = do
    x<- arbitrary
    return $ Identity x

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b) 
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x<- arbitrary
    y<- arbitrary
    return $ Two x y
 
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d) 
  
instance (Arbitrary a, Arbitrary b,Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    x<- arbitrary
    y<- arbitrary
    z<- arbitrary
    p<- arbitrary
    return $ Four x y z p
 
instance Functor (Four' a) where
  fmap f (Four' t y u b) = Four' t y u (f b) 
  
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    x<- arbitrary
    x'<- arbitrary
    x''<- arbitrary
    y<- arbitrary
    return $ Four' x x' x'' y
 
--Prelude> :{
--Main| let f :: [Int] -> Bool
--Main| f x = functorIdentity x
--Main| :}
--Prelude> quickCheck f
-- OK, passed 100 tests.
--Prelude> c = functorCompose (+1) (*2)
--Prelude> li x = c (x :: [Int])
--Prelude> quickCheck li
-- OK, passed 100 tests.

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)
instance Functor Possibly where
  fmap _ LolNope = LolNope 
  fmap f (Yeppers a) = Yeppers $ f a 



newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)
  
newtype K a b =
  K a
-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)
  
  
data EvilGoateeConst a b =
  GoatyConst b
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
-- You thought you'd escaped the goats
-- by now didn't you? Nope.
--No, it doesn’t do anything interesting. No magic here or in the
--previous exercise. If it works, you succeeded.
--5. Do you need something extra to make the instance work?CHAPTER 16. FUNCTOR 670

data LiftItOut f a =
  LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
  
data Parappa f g a =
  DaWrappa (f a) (g a)
instance (Functor f, Functor g ) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)
--7. Don’t ask for more type class instances than you need. You can
--let GHC tell you what to do.
data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)
data Notorious g o a t =
  Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt) 
--9. You’ll need to use recursion.
data List a =
  Nil
  | Cons a (List a)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a d) = Cons (f a) (fmap f d)
  
--10. A tree of goats forms a Goat-Lord, fearsome poly-creature.
data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)(GoatLord a)(GoatLord a)
  
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)
-- A VERITABLE HYDRA OF GOATS
--11. You’ll use an extra functor for this one, although your solution
--might do it monomorphically without using fmap. Keep in
--mind that you will probably not be able to validate this one in
--the usual manner. Do your best to make it work.2
data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)
instance Functor TalkToMe where 
  fmap _ Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read g) = Read (fmap f g)