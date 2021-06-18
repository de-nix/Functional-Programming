module Main where

import qualified Data.Map as Map
import           Lib
import           Types

main :: IO ()
main = do
        let state = State (Stack [DeclarationStmt "varf" StringType, AssignStmt "varf" (ValueExp $ StringValue "test.in"),OpenFile (VarExp "varf"), DeclarationStmt "varc" IntType, ReadFromFile (VarExp "varf") "varc",PrintStmt (VarExp "varc"), ReadFromFile (VarExp "varf") "varc",PrintStmt (VarExp "varc"),CloseFile (VarExp "varf")]) (Sym Map.empty) (Out []) (File Map.empty)
        putStrLn("Original State")
        print (state)
        putStrLn(".\n./\n./\n.")
        recExecute state
