module           Types where
import qualified Data.Map as Map
import           System.IO

data Type = BoolType | IntType | StringType deriving (Eq,Show)
data Value = BoolValue {getBool :: Bool} | IntValue {getInt :: Integer} | StringValue {getString ::String} deriving (Eq, Show)
data LogicOperator = OrOperator | AndOperator deriving (Eq,Show)
data Exp = ValueExp Value | VarExp String | LogicExp LogicOperator Value Value  deriving (Eq,Show)
data Stmt = DeclarationStmt String Type |AssignStmt String Exp | PrintStmt Exp | OpenFile Exp |ReadFromFile Exp String | CloseFile Exp | ComposeStmt Stmt Stmt deriving (Show)

setDefaultValue :: Type -> Value
setDefaultValue t = case t of
    IntType -> IntValue 0
    StringType -> StringValue ""
    BoolType -> BoolValue False

andFunc (BoolValue a) (BoolValue b) = BoolValue  (a && b)
andFunc _ _ = error "and needs two booleans"

orFunc (BoolValue a) (BoolValue b) = BoolValue (a || b)
orFunc _ _ = error "or needs two booleans"

eval :: Exp -> Map.Map String Value -> Value
eval (ValueExp val) sym = val
eval (VarExp varName) sym = sym Map.! varName
eval (LogicExp AndOperator x y) sym = andFunc x y
eval (LogicExp OrOperator x y) sym = orFunc x y


getFileHandler :: Value -> Map.Map String Value ->IO Handle
getFileHandler (StringValue x) sym = openFile x ReadMode
readFromFile :: Handle -> IO Value
readFromFile handler = do
	stringValue <- hGetLine handler
        let number = read stringValue ::Integer
        return $ IntValue number
