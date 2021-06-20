module Parser(func) where
import Text.Trifecta
import Text.Parsec.Combinator
stop :: Parser a
stop = unexpected "stop"

oneTwo = char '1' >> char '2'
oneTwo'' = char '1' >> char '2' >> eof
-- read two characters,
-- '1' and '2', then die
oneTwo' = oneTwo >> stop
testParse :: Parser Char -> IO ()
testParse p =
  
  print $ parseString p mempty "123"

func = testParse

one = char '1'
