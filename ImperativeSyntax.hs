module ImperativeSyntax where

-- Abstract Syntax Document

data Env = Vars [(String,Int)] [(String, Bool)] [(String, String)] [(String, Function)]
            | Error String -- Env Type contains Variable information / error
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
                    | Multiply
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

showEnv :: Env -> IO()
showEnv (Error s) = putStrLn ("Error: " ++ s)
showEnv (Vars i b s f) = putStrLn ("Ints: " ++ (showInts i) ++ "\nBools: " ++ (showBools b) ++ "\nStrings: " ++ (showStrings s))

showInts :: [(String,Int)] -> String
showInts [] = ""
showInts ((s,v):xs) = "(" ++ s ++ "," ++ (show v) ++ "), " ++ showInts xs

showBools :: [(String,Bool)] -> String
showBools [] = ""
showBools ((s,v):xs) = "(" ++ s ++ "," ++ (show v) ++ "), " ++ showBools xs

showStrings :: [(String,String)] -> String
showStrings [] = ""
showStrings ((s,v):xs) = "(" ++ s ++ "," ++ v ++ "), " ++ showStrings xs




-- Test Programs




-- Test String Function - function creates a birthday card given a number of "very"s (in v var) and a name (in name var) and saves it in card var
-- Contains testing for all CMD subtypes, Int subtypes of Variable, Constant, and substracting, Bool subtypes of Constant, Comparing ints, and operating on bools, String subtypes of Constant, Variable, and Concatenation
-- Specific things not tested - comparing strings, negating ints
birthdayCard :: Program
birthdayCard = Prog [(Func "Main" [(If (Resolve And (Not (BConstant False)) (BVar "f")) (Exec "BC" (Vars [("v", 3)] [] [("name","Matt")] [])) (Exec "BC" (Vars [("v", 5)] [] [("name","Lane")] [])))]), (Func "BC" [(Assign "card" (Sexpr (Concat (SConstant "Happy Birthday ") (SVar "name")))), (Assign "card" (Sexpr (Concat (SVar "card") (SConstant (",\nI hope you have a "))))), (While (CompareInt Greater (IVar "v") (IConstant 0)) (List [(Assign "card" (Sexpr (Concat (SVar "card") (SConstant ("very "))))),(Assign "v" (Iexpr (Simplify Subtract (IVar "v") (IConstant 1))))])), (Assign "card" (Sexpr (Concat (SVar "card") (SConstant ("good birthday")))))])]


-- Test String Comparison - function checks if the password variable equals "password"
passwordProgram :: Program
passwordProgram = Prog [Func "Main" [(If (IsEqual (SVar "password") (SConstant "password")) (Assign "message" (Sexpr (SConstant "Correct Password, Congratulations"))) (Assign "message" (Sexpr (SConstant "Incorrect Password, Sorry"))))]]


-- Bad Birthday Program 
birthdayCardBad :: Program
birthdayCardBad = Prog [(Func "Main" [(If (Resolve And (Not (BConstant False)) (BVar "f")) (Exec "BC" (Vars [] [] [] [])) (Exec "BC" (Vars [] [] [] [])))]), (Func "BC" [(Assign "card" (Sexpr (Concat (SConstant "Happy Birthday ") (SVar "name")))), (Assign "card" (Sexpr (Concat (SVar "card") (SConstant (",\nI hope you have a "))))), (While (CompareInt Greater (IVar "v") (IConstant 0)) (List [(Assign "card" (Sexpr (Concat (SVar "card") (SConstant ("very "))))),(Assign "v" (Iexpr (Simplify Subtract (IVar "v") (IConstant 1))))])), (Assign "card" (Sexpr (Concat (SVar "card") (SConstant ("good birthday")))))])]


-- Bad Program, attempts to run a nonexistent function
badFC :: Program
badFC = Prog [Func "Main" [(Exec "Test" (Vars [] [] [] [("Test", Func "Test" [(Assign "x" (Iexpr (IConstant 18)))])]))]]


passwordEnv :: Env
passwordEnv = (Vars [] [] [("password", "password")] [])

wrongPasswordEnv :: Env
wrongPasswordEnv = (Vars [] [] [("password", "salami")] [])

emptyEnv :: Env
emptyEnv = Vars [] [] [] []

birthdayEnv :: Env
birthdayEnv = Vars [] [("f", True)] [] []



testPassword :: IO()
testPassword = showEnv (program passwordProgram passwordEnv)

testWrongPassword :: IO()
testWrongPassword = showEnv (program passwordProgram wrongPasswordEnv)

testCardGood :: IO()
testCardGood = showEnv (program birthdayCard birthdayEnv)

testCardBad :: IO()
testCardBad = showEnv (program birthdayCardBad birthdayEnv)

testBadCall :: IO()
testBadCall = showEnv (program badFC emptyEnv)






-- Semantics Functions

-- program semantics should call function semantics on first function in array
program :: Program -> Env -> Env
program (Prog (x:xs)) (Error s) = Error s
program (Prog (x:xs)) (Vars i b s f) = function x (Vars i b s (f ++ (formatFunctions xs)))

formatFunctions :: [Function] -> [(String, Function)]
formatFunctions [] = []
formatFunctions (f@(Func s c):xs) = [(s, f)] ++ (formatFunctions xs)

-- function should go through commands one by one, piping output env of one command to another
function :: Function -> Env -> Env
function f (Error s) = Error s
function (Func n []) e = e
function (Func n (x:xs)) e = function (Func n xs) (cmd x e)


-- cmd should resolve if/while statements and assign variables
cmd :: Cmd -> Env -> Env
cmd c (Error s) = Error s
cmd (Assign n i) e = assignexpr n i e
cmd (If b ca cb) e = if boolexpr b e == Nothing
                        then Error "Cannot reference a nonexistent variable"
                        else if boolexpr b e == Just True
                            then cmd ca e
                            else cmd cb e
cmd (While b c) e = if boolexpr b e == Nothing
                        then Error "Cannot reference a nonexistent variable"
                        else if boolexpr b e == Just True
                            then cmd (While b c) (cmd c e)
                            else e
cmd (List []) e = e
cmd (List (x:xs)) e = cmd (List xs) (cmd x e)
cmd (Exec fname params@(Vars ip bp sp fp)) (Vars is bs ss fs) = if (lookup fname fs) == Nothing
                                                                    then Error "Cannot reference a nonexistent function"
                                                                    else let Just val = lookup fname fs
                                                                        in function val (Vars (is ++ ip) (bs ++ bp) (ss ++ sp) (fs ++ fp))


-- Assigns a variable an expression value.  Returns environment with changed variable.
assignexpr :: String -> Expr -> Env -> Env
assignexpr s (Iexpr e) env@(Vars is bs ss f) = if (intexpr e env) == Nothing
                                                then Error "Cannot reference a nonexistent variable"
                                                else if (lookup s is) == Nothing
                                                    then let Just val = intexpr e env
                                                        in (Vars (is ++ [(s,val)]) bs ss f)
                                                    else let Just val = intexpr e env
                                                        in (Vars (map (setIVar s val) is) bs ss f)
assignexpr s (Bexpr e) env@(Vars is bs ss f) = if (boolexpr e env) == Nothing
                                                then Error "Cannot reference a nonexistent variable"
                                                else if (lookup s bs) == Nothing
                                                    then let Just val = boolexpr e env
                                                        in (Vars is (bs ++ [(s,val)]) ss f)
                                                    else let Just val = boolexpr e env
                                                        in (Vars is (map (setBVar s val) bs) ss f)
assignexpr s (Sexpr e) env@(Vars is bs ss f) = if (strexpr e env) == Nothing
                                                then Error "Cannot reference a nonexistent variable"
                                                else if (lookup s ss) == Nothing
                                                    then let Just val = strexpr e env
                                                        in (Vars is bs (ss ++ [(s,val)]) f)
                                                    else let Just val = strexpr e env
                                                        in (Vars is bs (map (setSVar s val) ss) f)

-- Turns an Integer Expression into an Integer
intexpr :: IntExpr -> Env -> Maybe Int
intexpr i (Error s) = Nothing
intexpr (IConstant x) e = Just x
intexpr (IVar s) (Vars is bs ss fs) = lookup s is
intexpr (Simplify op int1 int2) e = if ((intexpr int1 e) == Nothing) || ((intexpr int2 e) == Nothing)
                                        then Nothing
                                        else let ((Just val1),(Just val2)) = ((intexpr int1 e),(intexpr int2 e))
                                            in Just (intoperator op (val1) (val2))
intexpr (Negative i) e = if ((intexpr i e) == Nothing)
                            then Nothing
                            else let (Just val) = (intexpr i e)
                                in Just (-val)



--Turns a Boolean Expression into a Boolean
boolexpr :: BoolExpr -> Env -> Maybe Bool
boolexpr b (Error s) = Nothing
boolexpr (BConstant x) e = Just x
boolexpr (BVar b) (Vars _ bs _ _) = (lookup b bs)
boolexpr (CompareInt op int1 int2) e = if ((intexpr int1 e) == Nothing) || ((intexpr int2 e) == Nothing)
                                        then Nothing
                                        else let ((Just val1),(Just val2)) = ((intexpr int1 e),(intexpr int2 e))
                                            in Just (cmpoperator op (val1) (val2))
boolexpr (IsEqual str1 str2) e = if((strexpr str1 e) == Nothing) || ((strexpr str2 e) == Nothing)
                                    then Nothing
                                    else let ((Just val1),(Just val2)) = ((strexpr str1 e),(strexpr str2 e))
                                        in Just (val1 == val2)
boolexpr (Resolve op b1 b2) e = if((boolexpr b1 e) == Nothing) || ((boolexpr b2 e) == Nothing)
                                    then Nothing
                                    else if (op == And)
                                        then let ((Just val1),(Just val2)) = ((boolexpr b1 e),(boolexpr b2 e))
                                            in Just (val1 && val2)
                                        else let ((Just val1),(Just val2)) = ((boolexpr b1 e),(boolexpr b2 e))
                                            in Just (val1 || val1)
boolexpr (Not b1) e = if (boolexpr b1 e) == Nothing
                        then Nothing
                        else let (Just val1) = (boolexpr b1 e)
                            in Just (not val1)

-- Turns a String expression into a String
strexpr :: StrExpr -> Env -> Maybe String
strexpr (SVar v) (Vars _ _ ss _) = (lookup v ss) -- search string array for v
strexpr (SConstant s)  _ = Just s
strexpr (Concat s1 s2) e = if ((strexpr s1 e == Nothing) || (strexpr s2 e == Nothing))
                            then Nothing
                            else let ((Just val1),(Just val2)) = ((strexpr s1 e),(strexpr s2 e))
                                in Just (val1 ++ val2)

-- Performs math on ints to return an int
intoperator :: IntOperator -> Int -> Int -> Int
intoperator Add int1 int2 = int1 + int2
intoperator Subtract int1 int2 = int1 - int2
intoperator Multiply int1 int2 = int1 * int2


-- Compares ints to return a bool
cmpoperator :: CmpOperator -> Int -> Int -> Bool
cmpoperator Equal int1 int2 = if int1 == int2 then True else False
cmpoperator Greater int1 int2 = if int1 > int2 then True else False
cmpoperator Less int1 int2 = if int1 < int2 then True else False


-- Static typing
data Type = TBool | TInt | TString | TError
      deriving (Eq,Show)

typeOfProgram :: Program -> Bool
typeOfProgram (Prog []) = True
typeOfProgram (Prog (x:xs)) = if (typeOfFunction x)
                                then typeOfProgram (Prog xs)
                                else False
                                
typeOfFunction :: Function -> Bool
typeOfFunction (Func _ []) = True
typeOfFunction (Func s (x:xs)) = if (typeOfCMD x)
                                 then typeOfFunction (Func s xs)
                                 else False
                                
                                
typeOfCMD :: Cmd -> Bool
typeOfCMD (If b c1 c2) = case (typeOfB b, typeOfCMD c1, typeOfCMD c2) of
                            (TBool, True, True)   -> True
                            _                     -> False
typeOfCMD (While b c) = case (typeOfB b, typeOfCMD c) of
                            (TBool, True)         -> True
                            _                     -> False
typeOfCMD (List [])    = True
typeOfCMD (List (x:xs)) = case (typeOfCMD x) of
                            True                  -> typeOfCMD (List xs)
                            _                     -> False
typeOfCMD (Exec s e) = True -- Running this command with anything other than a String and a valid Env Variable will result in a Haskell Type error, so we do not need to check at compile time.
typeOfCMD (Assign s e) = case (typeOfExpr e) of
                            TInt      -> True
                            TBool     -> True
                            TString   -> True
                            _         -> False

typeOfExpr :: Expr -> Type
typeOfExpr (Iexpr i)           = case (typeOfI i) of
                              TInt    -> TInt
                              _       -> TError
typeOfExpr (Sexpr s)           = case (typeOfS s) of
                              TString -> TString
                              _       -> TError
typeOfExpr (Bexpr b)           = case (typeOfB b) of
                              TBool   -> TBool
                              _       -> TError

typeOfI :: IntExpr -> Type
typeOfI (IVar s)           = TInt
typeOfI (IConstant i)      = TInt
typeOfI (Simplify _ i j)   = case (typeOfI i,typeOfI j) of
                               (TInt,TInt) -> TInt
                               _           -> TError
typeOfI (Negative i)       = case (typeOfI i) of
                               TInt        -> TInt
                               _           -> TError
_                          = TError

typeOfB :: BoolExpr -> Type
typeOfB (BVar b)           = TBool
typeOfB (BConstant b)      = TBool
typeOfB (CompareInt _ a b) = case (typeOfI a, typeOfI b) of
                               (TInt,TInt) -> TBool
                               _           -> TError
typeOfB (IsEqual s1 s2)    = case (typeOfS s1, typeOfS s2) of
                               (TString, TString) -> TBool
                               _                  -> TError
typeOfB (Resolve _ b1 b2)  = case (typeOfB b1, typeOfB b2) of
                               (TBool, TBool)     -> TBool
                               _                  -> TError
typeOfB (Not b)            = case (typeOfB b)     of
                               (TBool)            -> TBool
                               _                  -> TError

typeOfS :: StrExpr -> Type
typeOfS (SVar s)           = TString
typeOfS (SConstant s)      = TString
typeOfS (Concat s1 s2)     = case (typeOfS s1, typeOfS s2) of
                              (TString,TString) -> TString
                              _                 -> TError

-- program is made up of a main function with commands, and a list of functions
-- functions are made up of a list of commands
-- commands can modify variables, call functions
