

module ImperativeSyntax where

-- Abstract Syntax Document

data Env = Vars [(String,Int)] [(String, Bool)] [(String, String)] [(String, Function)]
            | Error -- Env Type contains Variable information / error
        deriving (Eq, Show)

data Program = Prog [Function] -- Program contains all syntax/code for any program
    deriving (Eq, Show)

data Function = Func String [Cmd] -- Function contains a list of commands that are called when a function is run and a name
    deriving (Eq, Show)

data Cmd = Assign String Expr  -- Can assign variables, run functions, or control program flow with if/while
            | If BoolExpr Cmd Cmd
            | While BoolExpr Cmd
            | List [Cmd]
                  | Exec String Env
    deriving (Eq, Show)

data Expr = Bexpr BoolExpr
                   | Iexpr IntExpr
                   | Sexpr StrExpr
    deriving (Eq, Show)

data IntExpr = IVar String  -- An expression that can be resolved to an int value.  Can be used to hold variables.
             | IConstant Int
             | Simplify IntOperator IntExpr IntExpr
             | Negative IntExpr
    deriving (Eq, Show)

data BoolExpr = BVar String
                | BConstant Bool  -- An expression that can be resloved to a bool value
                | CompareInt CmpOperator IntExpr IntExpr
                        | IsEqual StrExpr StrExpr
                | Resolve BoolOperator BoolExpr BoolExpr
                | Not BoolExpr
    deriving (Eq, Show)


data StrExpr = SVar String
                        | SConstant String
                        | Concat StrExpr StrExpr
    deriving (Eq, Show)


-- Operator Syntax Definitions

-- Comparison Operators for two Ints
data CmpOperator = Greater
                    | Less
                    | Equal
    deriving (Eq, Show)

-- And and Or Operators for two Booleans
data BoolOperator = And
                    | Or
    deriving (Eq, Show)

-- Arithmetic Operators for two Ints
data IntOperator = Add
                    | Subtract
                    | Multipy
                    | Divide
    deriving (Eq, Show)



-- Library Functions


-- Simple Print Function to show environment
-- showEnv :: Env -> String
-- showEnv Error = "Error"
-- showEnv (Vars a b c d) = "Ints: \n" ++ showArray a ++ "\nBools: \n" ++ showArray b ++ "\nStrings: \n" ++ showArray c

-- showArray :: [(String,b)] -> String
-- showArray [] = ""
-- showArray ((s,e):xs) = "(" ++ s ++ ", " ++ show e ++ ")" ++ showArray(xs)

-- Can Be Errored by searching for nonexistent variable
-- searchEnv :: Env -> String -> Bool
-- searchEnv (Vars []) s = False
-- searchEnv (Vars ((n,v):xs)) s = if s == n then True else searchEnv (Vars xs) s


-- Function to map across environment - takes string and intexpr - if string = variable name, change variable to have new value
setIVar :: String -> Int -> (String,Int) -> (String,Int)
setIVar newS newI (oldS,oldI) = if newS == oldS then (newS,newI) else (oldS,oldI)

setBVar :: String -> Bool -> (String,Bool) -> (String,Bool)
setBVar newS newI (oldS,oldI) = if newS == oldS then (newS,newI) else (oldS,oldI)

setSVar :: String -> String -> (String,String) -> (String,String)
setSVar newS newI (oldS,oldI) = if newS == oldS then (newS,newI) else (oldS,oldI)



-- Test Programs

-- Good Program, sets x to 5, then if x=5, sets x to 1
testGoodProgram :: Program
testGoodProgram = Prog [Func "Main" [Assign "x" (Iexpr (IConstant 5)), If (CompareInt Equal (IVar "x") (IConstant 5)) (Assign "x" (Iexpr (IConstant 1))) (Assign "x" (Iexpr (IConstant 10)))]]

-- Bad Program, sets x to 5, then if y=5, sets y to 5 - should return Error
testBadProgram :: Program
testBadProgram = Prog [Func "Main" [Assign "x" (Iexpr (IConstant 5)), If (CompareInt Equal (IVar "y") (IConstant 5)) (Assign "y" (Iexpr (IConstant 1))) (Assign "y" (Iexpr (IConstant 10)))]]

-- testLargeProgram :: Program
-- testLargeProgram = Prog [Func "Main" [(Assign "x" (Iexpr (IConstant 5))), (If (CompareInt Greater (IVar "x") (IConstant 3)) (Exec "F2" testParams1) (Exec "F2" testParams2)), (Assign "Count" (Iexpr (IVar "z"))),)


testEnv :: Env
testEnv = Vars [] [] [] []

goodTest :: String
goodTest = show (program testGoodProgram testEnv)

badTest :: String
badTest = show (program testBadProgram testEnv)





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
cmd (Assign n i) e = assignexpr n i e
cmd (If b ca cb) e = if boolexpr b e == Nothing
                        then Error
                        else if boolexpr b e == Just True
                            then cmd ca e
                            else cmd cb e
cmd (While b c) e = e -- todo - implement while loops with a recursive function
cmd (Exec fname params@(Vars ip bp sp fp)) (Vars is bs ss fs) = if (lookup fname fs) == Nothing
                                                                    then Error
                                                                    else let Just val = lookup fname fs
                                                                        in function val (Vars (is ++ ip) (bs ++ bp) (ss ++ sp) (fs ++ fp))


-- Assigns a variable an expression value.  Returns environment with changed variable.
assignexpr :: String -> Expr -> Env -> Env
assignexpr s (Iexpr e) env@(Vars is bs ss f) = if (intexpr e env) == Nothing
                                                then Error
                                                else if (lookup s is) == Nothing 
                                                    then let Just val = intexpr e env
                                                        in (Vars (is ++ [(s,val)]) bs ss f)
                                                    else let Just val = intexpr e env
                                                        in (Vars (map (setIVar s val) is) bs ss f)
assignexpr s (Bexpr e) env@(Vars is bs ss f) = if (boolexpr e env) == Nothing
                                                then Error
                                                else if (lookup s bs) == Nothing 
                                                    then let Just val = boolexpr e env
                                                        in (Vars is (bs ++ [(s,val)]) ss f)
                                                    else let Just val = boolexpr e env
                                                        in (Vars is (map (setBVar s val) bs) ss f)
assignexpr s (Sexpr e) env@(Vars is bs ss f) = if (strexpr e env) == Nothing
                                                then Error
                                                else if (lookup s ss) == Nothing 
                                                    then let Just val = strexpr e env
                                                        in (Vars is bs (ss ++ [(s,val)]) f)
                                                    else let Just val = strexpr e env
                                                        in (Vars is bs (map (setSVar s val) ss) f)

-- Turns an Integer Expression into an Integer
intexpr :: IntExpr -> Env -> Maybe Int
intexpr i Error = Nothing
intexpr (IConstant x) e = Just x
intexpr (IVar s) (Vars is bs ss fs) = lookup s is


--Turns a Boolean Expression into a Boolean
boolexpr :: BoolExpr -> Env -> Maybe Bool
boolexpr (BConstant x) e = Just x
boolexpr (CompareInt op int1 int2) e = if ((intexpr int1 e) == Nothing) || ((intexpr int2 e) == Nothing)
                                        then Nothing
                                        else let ((Just val1),(Just val2)) = ((intexpr int1 e),(intexpr int2 e))
                                            in Just (cmpoperator op (val1) (val2))

-- Turns a String expression into a String
strexpr :: StrExpr -> Env -> Maybe String
strexpr (SVar v) (Vars _ _ ss _) = (lookup v ss) -- search string array for v
strexpr (SConstant s)  _ = Just s
strexpr (Concat s1 s2) e = if ((strexpr s1 e == Nothing) || (strexpr s2 e == Nothing))
                            then Nothing
                            else let ((Just val1),(Just val2)) = ((strexpr s1 e),(strexpr s2 e))
                                in Just (val1 ++ val2)

-- Compares ints to return a bool
cmpoperator :: CmpOperator -> Int -> Int -> Bool
cmpoperator Equal int1 int2 = if int1 == int2 then True else False
cmpoperator Greater int1 int2 = if int1 > int2 then True else False
cmpoperator Less int1 int2 = if int1 < int2 then True else False



-- program is made up of a main function with commands, and a list of functions
-- functions are made up of a list of commands
-- commands can modify variables, call functions
