module Dice where
import System.Random
data Die
  = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show)

rollDie ::Int -> Die
rollDie a
  | a == 1 = DieOne
  | a == 2 = DieTwo
  | a == 3 = DieThree
  | a == 4 = DieFour
  | a == 5 = DieFive
  | a == 6 = DieSix
  | otherwise = error "erooor"

nextRoll :: StdGen -> (Int, StdGen)
nextRoll gen = randomR (1,6) gen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN x gen = go 0 0 gen
  where go sum count gen | sum >= x = count
                         | otherwise = let (die,newGen) = nextRoll gen in
                            go (sum + die) (count +1) newGen
----2. Change rollsToGetN to recording the series of die that occurred
----in addition to the count.
rollsCountLogged :: Int
  -> StdGen
  -> (Int, [Die])
rollsCountLogged x gen = go 0 (0,[]) gen where
                            go sum tup gen | sum >= x = tup
                                           | otherwise = let (die,nex) = nextRoll gen in
                                                        go (die + sum) (fst tup +1, sec tup ++ [rollDie die]) nex


--

--Write the following functions. You’ll want to use your own State type
--for which you’ve defned the Functor, Applicative, and Monad.
--1. Construct a State where the state is also the value you return.

newtype State s a = State {runState :: s -> (a,s)}

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a,s') = g s
                                    in (f a, s')
instance Applicative (State s) where
  State f <*> State g = State $ \s -> let (a,s') = f s
                                          (x,s'') = g s'
                                         in (a x, s'')
instance Monad (State s) where
  State x >>= f = State $ \s -> let (a, s') = x s
                                    (State f') = f a
                                 in f' s'

get :: State s s
get = State $ \x -> (x,x)
--Expected output
--Prelude> runState get "curryIsAmaze"
--("curryIsAmaze","curryIsAmaze")
--2. Construct a State where the resulting state is the argument
--provided and the value is defaulted to unit.
put :: s -> State s ()
put s = State $ const ((), s)run
--Prelude> runState (put "blah") "woot"
--((),"blah")
--3. Run the State with s and get the state that results.
exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)
--Prelude> exec (put "wilma") "daphne"
--"wilma"
--Prelude> exec get "scooby papu"
--"scooby papu"
--4. Run the State with �and get the value that results.
eval :: State s a -> s -> a
eval (State sa) = fst . sa
--Prelude> eval get "bunnicula"
--"bunnicula"
--Prelude> eval get "stake a bunny"
--"stake a bunny"
--5. Write a function which applies a function to create a new State.
modify :: (s -> s) -> State s ()
modify ff = State $ \x -> ((), ff x)
--Should behave like the following:
--Prelude> runState (modify (+1)) 0
--((),1)
--Prelude> runState (modify (+1) >> modify (+1)) 0
--((),2)
--You don’t need to compose them, you can throw away the result
--because it returns unit for �an
