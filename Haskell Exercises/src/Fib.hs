
module Fib  (
  fibonacci,
  mainFibo,
  fibo,
  she,
  encoding,
  decoding
  ) where
import Data.Char
fibonacci x y z | x <= y = z !! (y-x)
                |otherwise = fibonacci x (y+1) (( head z + z !! 1):z)
mainFibo x | x> 0 = fibonacci x 2 [1,0]
           | x<=0 = error "You should give a natural number"           
fibo 1 = 0
fibo 2 = 1
fibo x = fibo (x-1) + fibo (x-2)

she "" = []
she x= takeWhile (/='\n') x : ( she . (drop 1) $ dropWhile (/='\n') x)

shift ch | ch == 'z' = 'a'
         | ch >'a' && ch <='z' = succ ch
         | otherwise = ch
reverseShift ch 
                | ch == 'a' = 'z'
                | ch >'a' && ch <='z' = pred ch
                | otherwise = ch
         
encoding "" = []
encoding (x:xs) = (shift . shift . shift $ x) : (encoding  xs)
decoding "" = []
decoding (x:xs) = (reverseShift . reverseShift . reverseShift $ x) : (decoding xs) 