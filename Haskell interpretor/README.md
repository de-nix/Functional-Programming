#Haskell interpreter
#### This project simulates a compiler interpreter that given a stack of statements, it executes recursively each statement from the stack modifying a global state of the program consisting in a list of outputs, a symbol table, and a file table.


State {getPStack = Stack [DeclarationStmt "varf" StringType, <br/>
&nbsp;&nbsp;&nbsp;&nbsp; AssignStmt "varf" (ValueExp (StringValue {getString = "test.in"})), <br/>
&nbsp;&nbsp;&nbsp;&nbsp;  OpenFile (VarExp "varf"), DeclarationStmt "varc" IntType, <br/>
&nbsp;&nbsp;&nbsp;&nbsp;  ReadFromFile (VarExp "varf") "varc", PrintStmt (VarExp "varc"), <br/>
&nbsp;&nbsp;&nbsp;&nbsp; ReadFromFile (VarExp "varf") "varc",PrintStmt (VarExp "varc"), <br/>
&nbsp;&nbsp;&nbsp;&nbsp;   CloseFile (VarExp "varf")], getPSym = Sym {getSym = fromList[]}, <br/>
&nbsp;&nbsp;&nbsp;&nbsp;  getPOut = Out [], getPFile = File {getFile = fromList []}} <br/>
<br/>
After one execution goes in ->
<br/>
<br/>
State {getPStack = Stack [AssignStmt "varf" (ValueExp (StringValue {getString = "test.in"})), <br/>
&nbsp;&nbsp;&nbsp;&nbsp;   OpenFile (VarExp "varf"), DeclarationStmt "varc" IntType, <br/>
&nbsp;&nbsp;&nbsp;&nbsp;   ReadFromFile (VarExp "varf") "varc", PrintStmt (VarExp "varc"), <br/>
&nbsp;&nbsp;&nbsp;&nbsp;    ReadFromFile (VarExp "varf") "varc",PrintStmt (VarExp "varc"), <br/>
&nbsp;&nbsp;&nbsp;&nbsp;   CloseFile (VarExp "varf")],  <br/>
&nbsp;&nbsp;&nbsp;&nbsp;   getPSym = Sym {getSym = fromList [("varf",StringValue {getString =""})]}, <br/>
&nbsp;&nbsp;&nbsp;&nbsp;   getPOut = Out [], getPFile = File {getFile = fromList []}} <br/>
