module Main where
import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess)
import System.IO
  (BufferMode(NoBuffering),
  hSetBuffering,
  stdout)
import System.Random(randomRIO)

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 9

type WordList = [String]

gameWords :: IO WordList
gameWords = do
  dict <- readFile "words"
  let aw = (lines dict)
  return (filter gameLength aw)
    where gameLength w = let l = length (w :: String) in l >= minWordLength && l < maxWordLength

randomWord' :: IO String
randomWord' = do
  words <- gameWords
  randomIndex <- randomRIO (0 , (length words)-1)
  return $ words !! randomIndex
  --gameWords >>= randomWord if randomWord could take an argument 


data Puzzle = Puzzle String [Maybe Char] [Char]
instance Show Puzzle where
  show (Puzzle _ discovered guessed) = (intersperse ' ' $
    fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

setChar ::  String ->Char -> Maybe Char
setChar b a | elem a b = (Just a)
            | otherwise = Nothing

updatePuzzle :: Puzzle -> Char -> IO Puzzle
updatePuzzle a@(Puzzle d c e) b = return (Puzzle d (fmap (setChar (b:e)) d) (b:e))


freshPuzzle :: String -> Puzzle
freshPuzzle str= Puzzle str (fmap (setChar []) str) []


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed  (Puzzle _ _ a ) char = elem char a

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just a) = a

fillInCharacter :: Puzzle -> Char ->IO Puzzle
fillInCharacter game@(Puzzle a b c) char 
              |alreadyGuessed game char == False && elem char a ==True = do
                                                                                  putStrLn "This character was in the\
                                                                                  \ word, filling in the word\
                                                                                  \ accordingly"
                                                                                  puzzle <- (updatePuzzle game char)
                                                                                  return puzzle
              |alreadyGuessed game char == False = do 
                                                      putStrLn "This character wasn't in\
                                                      \ the word, try again."
                                                      return (Puzzle a b (char:c))
              |otherwise = do 
                            putStrLn "You already guessed that\
                            \ character, pick \
                            \ something else!"
                            return game



countJust :: [Char] -> [Char] -> Int
countJust ( a : rest) lst | (elem a lst )= 1 + (countJust rest lst)
                              | otherwise = (countJust rest lst)
countJust [] lst = 0

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filled guessed) =
  if (length guessed)-(countJust guessed wordToGuess) > 6 then
    do 
      putStrLn "You lose!"
      putStrLn $  "The word was: " ++ wordToGuess
      exitSuccess
  else return ()
  
gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar then
    do 
      putStrLn "You win!"
      exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle@(Puzzle a b c) = forever $ do
  gameOver puzzle
  gameWin puzzle
  
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStrLn $ "errors left : " ++ show (6 - (length c) + (countJust c a))
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> fillInCharacter puzzle c >>= runGame
    _ ->putStrLn "Your guess must be a single character"
    
main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle