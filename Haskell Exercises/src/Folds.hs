module Folds (filterDbNumber, sumF,avg) where

--import Data.Time
data DatabaseItem = DbString String
  | DbNumber Integer
--  | DbDate UTCTime
  deriving (Eq, Ord, Show)
theDatabase :: [DatabaseItem]
theDatabase =
  [ 
--  DbDate (UTCTime
--    (fromGregorian 1911 5 1)
--    (secondsToDiffTime 34123)),
    DbNumber 9001
    ,DbNumber 901
    ,DbString "Hello, my!"
    ,DbNumber 91
    ,DbString "Hello, world!"
--    , DbDate (UTCTime
--        (fromGregorian 1921 5 1)
--        (secondsToDiffTime 34123))
  ]
  
filterNumber (DbNumber x) = [x]
filterNumber _ = []

sumFilter (DbNumber x) = x
sumFilter _ = 0
nsumFilter (DbNumber x) = (x,1)
nsumFilter _ = (0,0)
--  filterDbDate :: [DatabaseItem]
--  -> [UTCTime]
--  filterDbDate = undefined
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = foldr (\a b -> (filterNumber a) ++ b) [] theDatabase
----  mostRecent :: [DatabaseItem]
----  -> UTCTime
----  mostRecent = undefined
--  sumDb :: [DatabaseItem]
--  -> Integer
--  sumDb = undefined
--  avgDb :: [DatabaseItem]
--  -> Double
--  avgDb = undefined

sumF xs = foldr (\a b -> (sumFilter a) + b) 0 xs
fff xs = foldr (\a b -> (fst b + fst (nsumFilter a) , snd b + snd (nsumFilter a))) (0,0) xs
avg xs = div (fst(fff xs)) (snd(fff xs))

stops = "pbtdkg"
vowels = "aeiou"
