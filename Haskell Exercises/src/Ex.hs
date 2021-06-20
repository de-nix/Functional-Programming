{-# LANGUAGE QuasiQuotes #-}
module Ex(someFunc) where
import           Control.Monad.Trans.State
import           Text.Trifecta
import           Text.Parser.Combinators
import           Text.RawString.QQ
import           Control.Applicative
import           Data.Ratio ((%))
eitherOr :: String
eitherOr = [r|
12.4
3/4
451.0
3.5
5/6
14
/7
15.4
|]

parserFraction :: Parser Rational
parserFraction = do
      numerator <- decimal
      char '/'
      dominator <- decimal
      case dominator of
        0 -> fail "denominator can't be 0"
        _ -> return $ numerator % dominator

parserReal :: Parser Float
parserReal = do
      first <- decimal
      char '.'
      point <- decimal
      return $ fromInteger first  + subunit (fromInteger point)

subunit dec | dec < 1 = dec
            | otherwise = subunit (dec / 10)

parseNos :: Parser RationalOrReal
parseNos = do
  skipMany (oneOf "\n")

  v <- try (Right <$> parserReal)
        <|> try (Left <$> parserFraction)
  skipMany (oneOf "\n")
  return v

someFunc =print $ parseString (some parseNos) mempty eitherOr


type NumberOrString =
  Either Integer String

type RationalOrReal =  Either Rational  Float
