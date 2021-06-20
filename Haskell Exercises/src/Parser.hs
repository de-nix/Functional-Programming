module Parser(func) where

import           Control.Monad.Trans.State
import           Text.Trifecta
import           Text.Parser.Combinators

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

multi :: String -> Parser [Char]
multi [] = return []
multi (x:xs) = do
    char x
    multi xs
    return (x:xs)


one' :: Parser b
one' = one >> stop

type Token = Char

type    Parser'   a =     String -> Maybe  (a,  String)
newtype Parser''  a = P ([Token] ->       [(a, [Token])])
type    Parser''' a =     String ->       [(a,  String)]

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

parseSt :: String -> Parser String
parseSt = string

oneTwo' :: Parser b
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

--p :: Parser String -> IO()
p parser = print $ parseString parser mempty "123"

pNL :: String -> IO ()
pNL s = putStrLn ('\n' : s)

testEOF :: Parser () -> IO ()
testEOF p = print $ parseString p mempty "12"

someFunc :: IO ()
someFunc = do
  pNL "stop:"
  testParse stop
  pNL "one:"
  testEOF $ one >> eof
  pNL "one':"
  testParse one'
  pNL "oneTwo:"
  testEOF$  oneTwo >> eof
  pNL "oneTwo':"
  testParse oneTwo'

  pNL "p123'1:"
  p $ multi "1"
  p $ multi "12"
  p $ multi "123"
  print $ parseString (
                        do
                          xx <- integer
                          x <- eof
                          return xx
                       )
                        mempty "123"


