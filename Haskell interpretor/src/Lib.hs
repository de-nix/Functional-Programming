module Lib where
import qualified Data.Map as Map
import           System.IO
import           Types

newtype Stack = Stack [Stmt] deriving (Show)
newtype Out = Out [Value] deriving (Show)
newtype Sym = Sym {getSym :: Map.Map String Value} deriving (Show)
newtype File = File {getFile :: Map.Map String Handle} deriving (Show)
data ProgramState = State {getPStack ::Stack, getPSym :: Sym, getPOut :: Out, getPFile:: File} deriving(Show)

 
execute :: ProgramState ->IO ProgramState
execute (State (Stack ((DeclarationStmt x y):xs)) (Sym sym) out file) = do 
                      return $ State (Stack xs) (Sym $ Map.insert x (setDefaultValue y) sym) out file
execute (State (Stack ((AssignStmt x y):xs)) (Sym sym) out file) = do
                      return $ State (Stack xs) (Sym $ Map.insert x (eval y sym) sym) out file
execute (State (Stack ((PrintStmt x):xs)) (Sym sym) (Out out) file) = do
                      return $ State (Stack xs) (Sym sym) (Out $ (eval x sym ) : out) file
execute (State (Stack ((OpenFile x):xs)) (Sym sym) out (File file)) = do
                      h <- getFileHandler (eval x sym) sym
                      return $ State (Stack xs) (Sym sym) out (File $ Map.insert (getString (eval x sym)) h file)
execute (State (Stack ((ReadFromFile x y):xs)) (Sym sym) out (File file)) = do
                      newValue <- readFromFile (file Map.! (getString (eval x sym)))
                      return $ State (Stack xs) (Sym $ Map.insert y newValue sym) out (File file)
execute (State (Stack ((CloseFile x):xs)) (Sym sym) out (File file)) = do
                      h <- getFileHandler (eval x sym) sym
                      return $ State (Stack xs) (Sym sym) out (File $ Map.delete (getString (eval x sym)) file)
execute (State (Stack ((ComposeStmt x y):xs)) sym out file) = do
                      return $ State (Stack $ x:y:xs) sym out file

recExecute ::ProgramState -> IO()
recExecute state@(State (Stack (x:xs)) _ _ _) = do
        next <- execute state
        print(next)
        putStrLn(".\n.\n.\n")
        recExecute next
recExecute state = putStrLn("Stop")
