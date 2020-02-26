

module ImperativeSyntax where

-- Abstract Syntax Document

data Env = Vars [(String,Int)] 
            | Error -- Env Type contains Variable information / error

data Program = Prog [Function] -- Program contains all syntax/code for any program

data Function = Func String [Cmd] -- Function contains a list of commands that are called when a function is run and a name

data Cmd = Assign String IntExpr  -- Can assign variables, run functions, or control program flow with if/while
            | If BoolExpr Cmd Cmd
            | While BoolExpr Cmd
            | List [Cmd]
            
             
data BoolExpr = BConstant Bool  -- An expression that can be resloved to a bool value
                | Compare CmpOperator IntExpr IntExpr
                | Resolve BoolOperator BoolExpr BoolExpr
                | Not BoolExpr

data IntExpr = Var String  -- An expression that can be resolved to an int value.  Can be used to hold variables.
                | IConstant Int
                | Simplify IntOperator IntExpr IntExpr
                | Negative IntExpr
                


-- Operator Syntax Definitions

-- Comparison Operators for two Ints
data CmpOperator = Greater
                    | Less
                    | Equal

-- And and Or Operators for two Booleans                    
data BoolOperator = And
                    | Or

-- Arithmetic Operators for two Ints                    
data IntOperator = Add
                    | Subtract
                    | Multipy
                    | Divide
 


-- Library Functions


-- Simple Print Function to show environment
showEnv :: Env -> String
showEnv Error = "Error"
showEnv (Vars []) = ""
showEnv (Vars ((s,e):xs)) = "(" ++ s ++ ", " ++ show e ++ ")" ++ showEnv(Vars xs)

-- Can Be Errored by searching for nonexistent variable
searchEnv :: Env -> String -> Maybe Int
searchEnv (Vars []) s = Nothing
searchEnv (Vars ((n,v):xs)) s = if s == n then Just v else searchEnv (Vars xs) s


-- Function to map across environment - takes string and intexpr - if string = variable name, change variable to have new value
setVar :: String -> Int -> (String,Int) -> (String,Int)
setVar newS newI (oldS,oldI) = if newS == oldS then (newS,newI) else (oldS,oldI)



 
-- Test Programs       
  
--should set x to 1  
testProgram :: Program
testProgram = Prog [Func "Main" [Assign "x" (IConstant 5), If (Compare Equal (Var "x") (IConstant 5)) (Assign "x" (IConstant 1)) (Assign "x" (IConstant 10))]]

--should set x to 5
testFunction :: Function
testFunction = Func "Main" [Assign "x" (IConstant 5)]

testEnv :: Env
testEnv = Vars [("test",2)]

testString :: String
testString = showEnv (program testProgram testEnv)





-- Semantics Functions

-- program semantics should call function semantics on first function in array
program :: Program -> Env -> Env
program (Prog (x:xs)) Error = Error
program (Prog (x:xs)) e = function x e

-- function should go through commands one by one, piping output env of one command to another
function :: Function -> Env -> Env
function f Error = Error
function (Func n []) e = e
function (Func n (x:xs)) e = function (Func n xs) (cmd x e)

-- cmd should resolve if/while statements and assign variables
cmd :: Cmd -> Env -> Env
cmd c Error = Error
cmd (Assign n i) (Vars e) = if (intexpr i (Vars e)) == Nothing
                        then Error
                        else if (searchEnv (Vars e) n) == Nothing
                            then let Just val = intexpr i (Vars e)
                                in Vars (e ++ [(n,val)])
                            else let Just val = intexpr i (Vars e)
                                in Vars (map (setVar n val) e)
cmd (If b ca cb) e = if boolexpr b e == Nothing
                        then Error
                        else if boolexpr b e == Just True 
                            then cmd ca e
                            else cmd cb e
cmd (While b c) e = e -- todo - implement while loops with a recursive function



-- Turns an Integer Expression into an Integer
intexpr :: IntExpr -> Env -> Maybe Int
intexpr i Error = Nothing
intexpr (IConstant x) e = Just x
intexpr (Var s) e = searchEnv e s


--Turns a Boolean Expression into a Boolean
boolexpr :: BoolExpr -> Env -> Maybe Bool
boolexpr (BConstant x) e = Just x
boolexpr (Compare op int1 int2) e = if ((intexpr int1 e) == Nothing) || ((intexpr int2 e) == Nothing)
                                        then Nothing
                                        else let ((Just val1),(Just val2)) = ((intexpr int1 e),(intexpr int2 e))
                                            in Just (cmpoperator op (val1) (val2))

                                    
-- Compares ints to return a bool
cmpoperator :: CmpOperator -> Int -> Int -> Bool
cmpoperator Equal int1 int2 = if int1 == int2 then True else False
cmpoperator Greater int1 int2 = if int1 > int2 then True else False
cmpoperator Less int1 int2 = if int1 < int2 then True else False



-- program is made up of a main function with commands, and a list of functions
-- functions are made up of a list of commands
-- commands can modify variables, call functions


