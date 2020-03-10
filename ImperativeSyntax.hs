

module ImperativeSyntax where

-- Abstract Syntax Document

data Env = Vars [(String,Int)] [(String, Bool)] [(String, String)]
            | Error -- Env Type contains Variable information / error
   deriving(Eq,Show)

data Program = Prog [Function] -- Program contains all syntax/code for any program

data Function = Func String [Cmd] -- Function contains a list of commands that are called when a function is run and a name

data Cmd = Assign String Expr  -- Can assign variables, run functions, or control program flow with if/while
            | If BoolExpr Cmd Cmd
            | While BoolExpr Cmd
            | List [Cmd]
			      | Exec String Env

data Expr = Bexpr BoolExpr
			       | Iexpr IntExpr
			       | Sexpr StrExpr

data IntExpr = IVar String  -- An expression that can be resolved to an int value.  Can be used to hold variables.
             | IConstant Int
             | Simplify IntOperator IntExpr IntExpr
             | Negative IntExpr

data BoolExpr = BVar String
                | BConstant Bool  -- An expression that can be resloved to a bool value
                | CompareInt CmpOperator IntExpr IntExpr
				        | IsEqual StrExpr StrExpr
                | Resolve BoolOperator BoolExpr BoolExpr
                | Not BoolExpr


data StrExpr = SVar String
				        | SConstant String
				        | Concat StrExpr StrExpr




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
searchEnv :: Env -> String -> Bool
searchEnv (Vars []) s = False
searchEnv (Vars ((n,v):xs)) s = if s == n then True else searchEnv (Vars xs) s


-- Function to map across environment - takes string and intexpr - if string = variable name, change variable to have new value
setVar :: String -> Int -> (String,Int) -> (String,Int)
setVar newS newI (oldS,oldI) = if newS == oldS then (newS,newI) else (oldS,oldI)




-- Test Programs

-- Good Program, sets x to 5, then if x=5, sets x to 1
testGoodProgram :: Program
testGoodProgram = Prog [Func "Main" [Assign "x" (IConstant 5), If (Compare Equal (Var "x") (IConstant 5)) (Assign "x" (IConstant 1)) (Assign "x" (IConstant 10))]]

-- Bad Program, sets x to 5, then if y=5, sets y to 5 - should return Error
testBadProgram :: Program
testBadProgram = Prog [Func "Main" [Assign "x" (IConstant 5), If (Compare Equal (Var "y") (IConstant 5)) (Assign "y" (IConstant 1)) (Assign "y" (IConstant 10))]]

testEnv :: Env
testEnv = Vars []

goodTest :: String
goodTest = showEnv (program testGoodProgram testEnv)

badTest :: String
badTest = showEnv (program testBadProgram testEnv)





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
cmd (Assign n i) (Vars is bs ss) = assignexpr n i e if (intexpr i (Vars e)) == Nothing
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
cmd (Exec String Env) e =



assignexpr :: Expr -> Env -> Env
assignexpr (Iexpr e) (Vars is bs ss)  = if (intexpr i (Vars e)) == Nothing
											then Error
										else if (searchEnv (Vare e) n) == Nothing


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

-- Turns a String expression into a String
strexpr :: StrExpr -> Env -> Maybe String
strexpr (SVar v) (_ _ ss _) = (lookup v ss) -- search string array for v
strexpr (SConstant s)  _ = Just s
strexpr (Concat s1 s2) _ = Just (s1 ++ s2)

-- Compares ints to return a bool
cmpoperator :: CmpOperator -> Int -> Int -> Bool
cmpoperator Equal int1 int2 = if int1 == int2 then True else False
cmpoperator Greater int1 int2 = if int1 > int2 then True else False
cmpoperator Less int1 int2 = if int1 < int2 then True else False



-- Static typing
data Type = TBool | TInt | TString | TError
deriving (Eq,Show)

typeOf :: Expr -> Type
typeOf Iexpr i = case (typeofI i) of
                  TInt -> TInt
                  _    -> TError
typeOf Sexpr s = case (typeOfS s) of
                  TString -> TString
                  _       -> TError
typeOf Bexpr b = case (typeOfB b) of
                  TBool   -> TBool
                  _       -> TError

typeofI :: Iexpr -> Type
typeOfI (IVar s)         = case (typeOf s) of
                          TString     -> TInt
                          _           -> TError
typeOfI (IConstant i)    = case (typeOf i) of
                          TInt        -> TInt
                          _           -> TError
typeOfI (Simplify IntOperator i j) = case (typeOf i,typeOf j) of
                                    (TInt,TInt) -> TInt
                                    _           -> TError
typeOfI (Negative i)     = case (typeOf i) of
                          TInt        -> TInt
                          _           -> TError
_                      = TError

-- program is made up of a main function with commands, and a list of functions
-- functions are made up of a list of commands
-- commands can modify variables, call functions
