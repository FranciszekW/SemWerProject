-- TODOS:
-- > Static type checking (checking execution mode in every evaluation function).
-- Think about if it's reasonable to reuse the same monad for both type checking and runtime.
-- > Check some strange swapping behaviour (test.txt).
-- > Fix grammar to allow definition or special statements to be the last instruction in a sequence.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude

import System.IO (readFile)
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO.Unsafe   ( unsafePerformIO )
import Control.Monad      ( when, ap, liftM )
import Control.Monad.Reader ( Reader, ReaderT, MonadReader, MonadIO, runReader, runReaderT, ask, local, liftIO, ap, liftM, lift )
import Control.Monad.State  ( State, StateT, MonadState, MonadIO, evalState, evalStateT, get, put, liftIO, ap, liftM, lift )
import Control.Monad.Except ( ExceptT, MonadError, MonadIO, runExceptT, throwError, catchError, liftIO, ap, liftM, lift )
import Control.Monad.Identity ( Identity, runIdentity, ap, liftM )


import Data.Map
import qualified GHC.Integer (leInteger) 

-- Syntactic categories given in the FraJer.cf file
import FraJer.Abs   ( SType(..), FType(..), Ident(..), Expr(..), Args(..), Params(..),
                      Instr(..), Def(..), Stmt(..), SpecStmt(..), Lambda(..) )
import FraJer.Lex   ( Token, mkPosToken )
import FraJer.Par   ( pExpr, pInstr, pDef, pStmt, pLambda, myLexer )
import FraJer.Print ( Print, printTree )
import FraJer.Skel  ()

mapGet :: (Ord k) => (Map k v) -> k -> v
mapGet map arg = map ! arg

mapSet :: (Ord k) => (Map k v) -> k -> v -> (Map k v)
mapSet map arg val = insert arg val map

mapHasKey :: (Ord k) => (Map k v) -> k -> Bool
mapHasKey map arg = member arg map

------------------------------------------ TYPES (sic!) -------------------------------------------

-- SType and FType are types from Frajer.cf.
data Type = SType | FType | TComplex ComplexType | TFunction DetailedFuncType deriving (Show, Eq)

-- Complex types (arrays and dictionaries)
data ComplexType = TArray SType | TDict SType deriving (Show, Eq)

data FuncParam = PSimple SType Ident | PFunc FType Ident deriving (Show, Eq)

-- Function types (can take functions as arguments, but return only simple types)
data DetailedFuncType = DetFunc [FuncParam] SType deriving (Show, Eq)

typeof :: SimpleValue -> Type
typeof (VInt _) = SType
typeof (VBool _) = SType

------------------------------------------ DATATYPES ----------------------------------------------

-- Simple values (Int, Bool)
data SimpleValue = VInt Integer | VBool Bool deriving (Show)

instance Eq SimpleValue where
  VInt x == VInt y = x == y
  VBool x == VBool y = x == y
  _ == _ = False

-- Complex values (arrays, dictionaries)
data ComplexValue = VArray Arr | VDict Dict deriving (Show)

-- Array of simple values (not used in functions directly)
type Arr = [SimpleValue]

-- Dictionary mapping keys to simple values (not used in functions directly)
-- We only have integers as keys in our language, since we don't have strings
-- and booleans don't make much sense as keys.
type DictKey = Integer
type Dict = Map DictKey SimpleValue

-- Value can either be a simple value (Int/Bool) or a complex value (Array/Dict)
data Value = SimpleVal SimpleValue | ComplexVal ComplexValue deriving (Show)

-- Function arguments can be either simple values (Int, Bool) or functions themselves
data FuncArg = SimpleArg SimpleValue | FArg MFunc

-- Function takes a list of function arguments and a store, returns a new store and a simple value)
type Func = [FuncArg] -> Store -> MyExprMonad

data Error =  DivByZero
            | ModByZero
            | KeyNotInDict DictKey
            | FunctionNotInScope Ident
            | IndexOutOfBounds Integer
            | InvalidArraySize Integer
            | InvalidBreakArgument Integer
            | InvalidContinueArgument Integer
            | TypeMismatch Type Type
            | VariableNotDefined Ident
            | CustomError String

instance Show Error where
    show DivByZero = "Division by zero"
    show ModByZero = "Modulo by zero"
    show (KeyNotInDict k) = "Key " ++ show k ++ " not in dictionary"
    show (FunctionNotInScope (Ident f)) = "Function " ++ f ++ " not in scope"
    show (IndexOutOfBounds i) = "Index " ++ show i ++ " out of bounds"
    show (InvalidArraySize s) = "Invalid array size: " ++ show s
    show (InvalidBreakArgument n) = "Invalid break argument: " ++ show n
    show (InvalidContinueArgument n) = "Invalid continue argument: " ++ show n
    show (TypeMismatch t1 t2) = "Type mismatch: " ++ show t1 ++ " and " ++ show t2
    show (VariableNotDefined (Ident var)) = "Variable " ++ var ++ " not defined"

------------------------------------------ ENVIRONMENTS -------------------------------------------

type Var = String
type Loc = Integer
type DebugFlag = Bool
type BreakCount = Integer
-- ContinueFlag is simply a bool, because continue outer n is equivalent to n breaks and then continue.
type ContinueFlag = Bool

type ControlFlow = (BreakCount, ContinueFlag)

-- One for assignment of a variable and one for reading a variable.
type DebugFlags = (DebugFlag, DebugFlag)

type VEnv = Map Var (Loc, DebugFlags)
type FEnv = Map Var Func

data Store = CStore {currMap :: Map Loc Value, nextLoc :: Loc} deriving Show

data StmtResult = StoreOnly Store | StoreAndValue Store SimpleValue deriving Show
type StmtState = (StmtResult, ControlFlow)

data ExecutionMode = TypeCheck | Runtime deriving (Show, Eq)
data TypeStore = TStore {typeMap :: Map Var Type, nextTypeLoc :: Loc} deriving Show

------------------------------------------ MONADS -------------------------------------------------

type MyExprMonad = Either Error (Store, SimpleValue)
type MFunc = [FuncArg] -> WorkingMonad (SimpleValue)
type FMEnv = Map Var MFunc
type Env = (VEnv, FMEnv, ExecutionMode)

-- TODO: Change the reader to use Env instead of (VEnv, FMEnv). Also, change the state
-- to use (Store, TypeStore, ControlFlow). Will do this later,
-- because it's a lot of work and we want the code to compile for now :)
newtype WorkingMonad a = WorkingMonad { runWorkingMonad :: ExceptT Error (ReaderT (VEnv, FMEnv) (StateT (Store, ControlFlow) IO)) a }

instance MonadState (Store, ControlFlow) WorkingMonad where
    get = WorkingMonad $ lift get
    put = WorkingMonad . lift . put

instance MonadError Error WorkingMonad where
    throwError = WorkingMonad . throwError
    catchError (WorkingMonad action) handler =
        WorkingMonad $ catchError action (runWorkingMonad . handler)

instance MonadReader (VEnv, FMEnv) WorkingMonad where
    ask = WorkingMonad ask
    local f (WorkingMonad action) = WorkingMonad $ local f action

instance Functor WorkingMonad where
    fmap = liftM

instance Applicative WorkingMonad where
    pure = return
    (<*>) = ap

instance Monad WorkingMonad where
    return = WorkingMonad . return
    (WorkingMonad action) >>= f = WorkingMonad (action >>= runWorkingMonad . f)

instance MonadIO WorkingMonad where
    liftIO = WorkingMonad . liftIO

instance MonadFail WorkingMonad where
    fail msg = throwError (CustomError (msg))


instance MonadFail (Either Error) where
  fail s = Left (error s)

------------------------------------------ HELPER FUNCTIONS ---------------------------------------
-- monads

-- Not working yet, because we need to change the reader to use Env instead of (VEnv, FMEnv).
--getExecutionMode :: WorkingMonad ExecutionMode
--getExecutionMode = WorkingMonad $ do
--    (_, _, mode) <- ask
--    return mode

getStore :: WorkingMonad Store
getStore = WorkingMonad $ do
    (sto, _) <- get
    return sto

getControlFlow :: WorkingMonad ControlFlow
getControlFlow = WorkingMonad $ do
    (_, controlFlow) <- get
    return controlFlow

getBreakCount :: WorkingMonad Integer
getBreakCount = WorkingMonad $ do
    (breakCount, _) <- runWorkingMonad getControlFlow
    return breakCount

getContinueFlag :: WorkingMonad Bool
getContinueFlag = WorkingMonad $ do
    (_, continueFlag) <- runWorkingMonad getControlFlow
    return continueFlag

putStore :: Store -> WorkingMonad ()
putStore sto = do
    (_, controlFlow) <- get
    put (sto, controlFlow)

putControlFlow :: ControlFlow -> WorkingMonad ()
putControlFlow controlFlow = do
    (sto, _) <- get
    put (sto, controlFlow)

msetVarVal :: Var -> Value -> WorkingMonad ()
msetVarVal var val = do
    (rhoV, _) <- ask
    sto <- getStore
    putStore (setVarVal rhoV sto var val)

-- not monadic:

unsafePrint s = unsafePerformIO (putStrLn s) `seq` ()

newloc:: Store -> (Loc, Store)
newloc (CStore map loc) = (loc, CStore map (loc + 1))

setVarLoc:: VEnv -> Var -> Loc -> VEnv
setVarLoc rhoV var loc =
    let (_, debugFlags) = mapGet rhoV var in
        mapSet rhoV var (loc, debugFlags)

-- in get VarVal we print the value if the read flag is set. At the end we return the value.
-- but we need to check the flag and if its set, print the value AND return it.
getVarVal:: VEnv -> Store -> Var -> Value
getVarVal rhoV sto var =
    let (loc, (readFlag, _)) = mapGet rhoV var in
    let val = mapGet (currMap sto) loc in
        if readFlag then
            unsafePrint ("Reading: " ++ show var ++ " = " ++ show val) `seq` val
        else
            val

--getVarVal:: VEnv -> Store -> Var -> Value
--getVarVal rhoV sto var =
--  let loc = mapGet rhoV var in
--    mapGet (currMap sto) loc

-- in set VarVal we print the value if the write flag is set

-- Similar to getVarVal, but we need to check the write flag and if its set, print the value.
-- At the end we return the new store.
setVarVal:: VEnv -> Store -> Var -> Value -> Store
setVarVal rhoV sto var val =
    let (loc, (_, writeFlag)) = mapGet rhoV var in
    let map = mapSet (currMap sto) loc val in
        if writeFlag then
            unsafePrint ("Writing: " ++ show var ++ " = " ++ show val) `seq` CStore map (nextLoc sto)
        else
            CStore map (nextLoc sto)
--
--setVarVal:: VEnv -> Store -> Var -> Value -> Store
--setVarVal rhoV sto var val =
--  let loc = mapGet rhoV var in
--  let map = mapSet (currMap sto) loc val in
--    CStore map (nextLoc sto)

replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs n newVal = Prelude.take n xs ++ [newVal] ++ Prelude.drop (n + 1) xs

------------------------------------------ SEMANTICS ----------------------------------------------

-----------------------------------------EXPRESSIONS (monadic) ----------------------------------------------

eMe :: Expr -> WorkingMonad SimpleValue

eMe (ENum n) = return (VInt n)

eMe (FuncVal (Ident func) args) = do
    f <- mgetfunc (Ident func)
    arguments <- eMa args
    res <- f arguments
    return res

eMe (VarVal (Ident var)) = do
    (rhoV, _) <- ask
    sto <- getStore
    case Data.Map.lookup var rhoV of
        Just (loc, _) -> case (mapGet (currMap sto) loc) of 
            SimpleVal val -> return val
            ComplexVal val -> throwError (TypeMismatch SType (TComplex (TArray STInt))) -- TODO set an actual type, not always array of ints
        Nothing -> throwError (VariableNotDefined (Ident var))

eMe (EPlus exp0 exp1) = do
    (VInt x) <- eMe exp0
    (VInt y) <- eMe exp1
    return (VInt (x + y))

eMe (EMinus exp0 exp1) = do
    (VInt x) <- eMe exp0
    (VInt y) <- eMe exp1
    return (VInt (x - y))

eMe (EDiv exp0 exp1) = do
    (VInt x) <- eMe exp0
    (VInt y) <- eMe exp1
    if y == 0 then throwError DivByZero
    else return (VInt (x `div` y))

eMe (EMul exp0 exp1) = do
    (VInt x) <- eMe exp0
    (VInt y) <- eMe exp1
    return (VInt (x * y))

eMe (EMod exp0 exp1) = do
    (VInt x) <- eMe exp0
    (VInt y) <- eMe exp1
    if y == 0 then throwError ModByZero
    else return (VInt (x `mod` y))

eMe (EArray (Ident arr) exp0) = do
    (VInt i) <- eMe exp0
    a <- mgetarray (Ident arr)
    if i < 0 || i >= toInteger (length a) then throwError (IndexOutOfBounds i)
    else return (a !! fromInteger i)

eMe (EDict (Ident dict) exp0) = do
    (VInt i) <- eMe exp0
    d <- mgetdict (Ident dict)
    if not (mapHasKey d i) then throwError (KeyNotInDict i)
    else return (d ! i)

eMe (EPostInc (Ident var)) = do
    (VInt x) <- eMe (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x + 1)))
    return (VInt x)

eMe (EPreInc (Ident var)) = do
    (VInt x) <- eMe (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x + 1)))
    return (VInt (x + 1))

eMe (EPostDec (Ident var)) = do
    (VInt x) <- eMe (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x - 1)))
    return (VInt x)

eMe (EPreDec (Ident var)) = do
    (VInt x) <- eMe (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x - 1)))
    return (VInt (x - 1))

--boools:
eMe (BTrue) = return (VBool True)
eMe (BFalse) = return (VBool False)

eMe (EEq exp0 exp1) = monadicEvalBinarySimplevalOp (==) exp0 exp1
eMe (ENeq exp0 exp1) = monadicEvalBinarySimplevalOp (/=) exp0 exp1
eMe (ELt exp0 exp1) = monadicEvalBinaryIntOp (<) exp0 exp1
eMe (EGt exp0 exp1) = monadicEvalBinaryIntOp (>) exp0 exp1
eMe (ELeq exp0 exp1) = monadicEvalBinaryIntOp (<=) exp0 exp1
eMe (EGeq exp0 exp1) = monadicEvalBinaryIntOp (>=) exp0 exp1

eMe (BNot exp0) = do
    (VBool x) <- eMe exp0
    return (VBool (not x))

eMe (BOr exp0 exp1) = monadicEvalBinaryBoolOp (||) exp0 exp1
eMe (BAnd exp0 exp1) = monadicEvalBinaryBoolOp (&&) exp0 exp1
eMe (BXor exp0 exp1) = monadicEvalBinaryBoolOp (/=) exp0 exp1

eMe (BDictHasKey (Ident dict) exp0) = do
    (VInt i) <- eMe exp0
    (rhoV, _) <- ask
    sto <- getStore
    let ComplexVal (VDict d) = getVarVal rhoV sto dict
    return (VBool (mapHasKey d i))

-- helper functions:

monadicEvalBinaryIntOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
monadicEvalBinaryIntOp op exp0 exp1 = do
    valX <- eMe exp0
    valY <- eMe exp1
    case (valX, valY) of
        (VInt x, VInt y) -> return (VBool (x `op` y))
        _ -> throwError $ TypeMismatch (typeof (valX)) (typeof (valY))

monadicEvalBinaryBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
monadicEvalBinaryBoolOp op exp0 exp1 = do
    valX <- eMe exp0
    valY <- eMe exp1
    case (valX, valY) of
        (VBool x, VBool y) -> return (VBool (x `op` y))
        _ -> throwError $ TypeMismatch (typeof valX) (typeof valY)


monadicEvalBinarySimplevalOp :: (SimpleValue -> SimpleValue -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
monadicEvalBinarySimplevalOp op exp0 exp1 = do
    x <- eMe exp0
    y <- eMe exp1
    return (VBool (x `op` y))

mgetfunc :: Ident -> WorkingMonad MFunc
mgetfunc (Ident func) = do
    (_, rhoF) <- ask
    case Data.Map.lookup func rhoF of
        Just f -> return f
        Nothing -> throwError (FunctionNotInScope (Ident func))

mgetarray :: Ident -> WorkingMonad Arr
mgetarray (Ident arr) = do
    (rhoV, _) <- ask
    case Data.Map.lookup arr rhoV of
        Just (loc, _) -> do
            sto <- getStore
            let ComplexVal (VArray a) = getVarVal rhoV sto arr
            return a
        Nothing -> throwError (VariableNotDefined (Ident arr))

mgetdict :: Ident -> WorkingMonad Dict
mgetdict (Ident dict) = do
    (rhoV, _) <- ask
    case Data.Map.lookup dict rhoV of
        Just (loc, _) -> do
            sto <- getStore
            let ComplexVal (VDict d) = getVarVal rhoV sto dict
            return d
        Nothing -> throwError (VariableNotDefined (Ident dict))


-----------------------------------------EXPRESSIONS ----------------------------------------------

-- Expressions can modify store too, because functions can be called.
eE :: Expr -> VEnv -> FEnv -> Store -> MyExprMonad

{-
FuncVal.   Expr2 ::= Ident "(" Args ")";
VarVal.    Expr2 ::= Ident;
-}

eE (ENum n) rhoV rhoF sto = Right (sto, VInt n)

eE (FuncVal (Ident func) args) rhoV rhoF sto = do
    let f = mapGet rhoF func
    let argsRes = eA args rhoV rhoF sto
    case argsRes of
        Left err -> Left err
        Right (sto', args') -> f args' sto'

eE (VarVal (Ident var)) rhoV rhoF sto = Right (sto, x) where
  SimpleVal x = getVarVal rhoV sto var

{-
EPlus.   Expr  ::= Expr "+" Expr1;
EMinus.  Expr  ::= Expr "-" Expr1;
EDiv.    Expr1 ::= Expr1 "/" Expr2;
EMul.    Expr1 ::= Expr1 "*" Expr2;
EMod.    Expr1 ::= Expr1 "%" Expr2;
ENum.    Expr2 ::= Integer;

EArray.  Expr2 ::= Ident "[" Expr "]";
EDict.   Expr2 ::= Ident "get" "[" Expr "]";
-}


eE (EPlus exp0 exp1) rhoV rhoF sto = evalBinaryOp (+) exp0 exp1 rhoV rhoF sto

eE (EMinus exp0 exp1) rhoV rhoF sto = evalBinaryOp (-) exp0 exp1 rhoV rhoF sto

eE (EDiv exp0 exp1) rhoV rhoF sto = do
    (sto', VInt x) <- eE exp0 rhoV rhoF sto
    (sto'', VInt y) <- eE exp1 rhoV rhoF sto'
    if y == 0 then Left DivByZero
    else return (sto'', VInt (x `div` y))

eE (EMul exp0 exp1) rhoV rhoF sto = evalBinaryOp (*) exp0 exp1 rhoV rhoF sto

eE (EMod exp0 exp1) rhoV rhoF sto = do
    (sto', VInt x) <- eE exp0 rhoV rhoF sto
    (sto'', VInt y) <- eE exp1 rhoV rhoF sto'
    if y == 0 then Left DivByZero
    else return (sto'', VInt (x `mod` y))

eE (EArray (Ident arr) exp0) rhoV rhoF sto = do
        let ComplexVal (VArray a) = getVarVal rhoV sto arr
        (sto', VInt i) <- eE exp0 rhoV rhoF sto

        if i < 0 || i >= toInteger (length a) then Left (IndexOutOfBounds i)
        else return (sto', a !! fromInteger i)

eE (EDict (Ident dict) exp0) rhoV rhoF sto = do
        let ComplexVal (VDict d) = getVarVal rhoV sto dict
        (sto', VInt i) <- eE exp0 rhoV rhoF sto

        if not (mapHasKey d i) then Left (KeyNotInDict i)
        else return (sto', d ! i)


-- Expressions with side effects.
{-
EPostInc. Expr2 ::= Ident "++";
EPreInc.  Expr2 ::= "++" Ident;
EPostDec. Expr2 ::= Ident "--";
EPreDec.  Expr2 ::= "--" Ident;
-}

eE (EPostInc (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x + 1))) in
        Right (sto', VInt x)

eE (EPreInc (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x + 1))) in
        Right (sto', VInt (x + 1))

eE (EPostDec (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x - 1))) in
        Right (sto', VInt x)

eE (EPreDec (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x - 1))) in
        Right (sto', VInt (x - 1))


-- Semantics of boolean expressions
{-
BTrue.  Expr2 ::= "true";
BFalse. Expr2 ::= "false";
EEq.    Expr1 ::= Expr1 "==" Expr2;
ENeq.   Expr1 ::= Expr1 "!=" Expr2;
ELt.    Expr1 ::= Expr1 "<" Expr2;
EGt.    Expr1 ::= Expr1 ">" Expr2;
ELeq.   Expr1 ::= Expr1 "<=" Expr2;
EGeq.   Expr1 ::= Expr1 ">=" Expr2;

BNot.   Expr2 ::= "!" Expr2;
-- && and || have the same priority (according to prof. Urzyczyn), but lower than comparison operators
BOr.    Expr ::= Expr "or" Expr1;
BAnd.   Expr ::= Expr "and" Expr1;
BXor.   Expr ::= Expr "xor" Expr1;

BDictHasKey.  Expr2 ::= Ident "has" "key" "[" Expr "]";
-}

eE (BTrue) rhoV rhoF sto = Right (sto, VBool True)
eE (BFalse) rhoV rhoF sto = Right (sto, VBool False)

-- Works for both integers and booleans
eE (EEq exp0 exp1) rhoV rhoF sto = evalSimpleValBinOp (==) exp0 exp1 rhoV rhoF sto
eE (ENeq exp0 exp1) rhoV rhoF sto = evalSimpleValBinOp (/=) exp0 exp1 rhoV rhoF sto

-- These work only for integers
eE (ELt exp0 exp1) rhoV rhoF sto = evalIntBinOp (<) exp0 exp1 rhoV rhoF sto
eE (EGt exp0 exp1) rhoV rhoF sto = evalIntBinOp (>) exp0 exp1 rhoV rhoF sto
eE (ELeq exp0 exp1) rhoV rhoF sto = evalIntBinOp (<=) exp0 exp1 rhoV rhoF sto
eE (EGeq exp0 exp1) rhoV rhoF sto = evalIntBinOp (>=) exp0 exp1 rhoV rhoF sto

-- Now specific to booleans
-- Not is a little special :)
eE (BNot exp0) rhoV rhoF sto = do
    (sto', VBool x) <- eE exp0 rhoV rhoF sto
    return (sto', VBool (not x))

eE (BOr exp0 exp1) rhoV rhoF sto = evalBoolBinOp (||) exp0 exp1 rhoV rhoF sto
eE (BAnd exp0 exp1) rhoV rhoF sto = evalBoolBinOp (&&) exp0 exp1 rhoV rhoF sto
eE (BXor exp0 exp1) rhoV rhoF sto = evalBoolBinOp (/=) exp0 exp1 rhoV rhoF sto

eE (BDictHasKey (Ident dict) exp0) rhoV rhoF sto = do
        let ComplexVal (VDict d) = getVarVal rhoV sto dict
        (sto', VInt i) <- eE exp0 rhoV rhoF sto
        return (sto', VBool (mapHasKey d i))

-- Helper functions
evalBinaryOp :: (Integer -> Integer -> Integer) -> Expr -> Expr -> VEnv -> FEnv -> Store -> MyExprMonad
evalBinaryOp op exp0 exp1 rhoV rhoF sto = do
    (sto', VInt x) <- eE exp0 rhoV rhoF sto
    (sto'', VInt y) <- eE exp1 rhoV rhoF sto'
    return (sto'', VInt (x `op` y))

evalBoolBinOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> VEnv -> FEnv -> Store -> MyExprMonad
evalBoolBinOp op exp0 exp1 rhoV rhoF sto = do
    (sto', VBool x) <- eE exp0 rhoV rhoF sto
    (sto'', VBool y) <- eE exp1 rhoV rhoF sto'
    return (sto'', VBool (x `op` y))

evalIntBinOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> VEnv -> FEnv -> Store -> MyExprMonad
evalIntBinOp op exp0 exp1 rhoV rhoF sto = do
    (sto', VInt x) <- eE exp0 rhoV rhoF sto
    (sto'', VInt y) <- eE exp1 rhoV rhoF sto'
    return (sto'', VBool (x `op` y))

evalSimpleValBinOp :: (SimpleValue -> SimpleValue -> Bool) -> Expr -> Expr -> VEnv -> FEnv -> Store -> MyExprMonad
evalSimpleValBinOp op exp0 exp1 rhoV rhoF sto = do
    (sto', x) <- eE exp0 rhoV rhoF sto
    (sto'', y) <- eE exp1 rhoV rhoF sto'
    return (sto'', VBool (x `op` y))

------------------------------------------ INSTRUCTIONS (monadic) -------------------------------------------
iMI :: Instr -> WorkingMonad (Maybe SimpleValue)

iMI (ISeq instr0 instr1) = do
    res <- iMI instr0
    case res of
        Just val -> return (Just val)
        Nothing -> do
            (breakCount, continueFlag) <- getControlFlow
            if breakCount > 0 || continueFlag then
                return Nothing
            else do
                iMI instr1

iMI (IDSeq def instr) = do
    (rhoV', rhoF') <- iMD def
    
    res <- local (const (rhoV', rhoF')) $ do
        iMI instr
    case res of
        Just val -> return (Just val)
        Nothing -> return Nothing

iMI (ISSeq specStmt instr) = do
    (rhoV', rhoF') <- iMSpecS specStmt

    res <- local (const (rhoV', rhoF')) $ do
        iMI instr
    case res of
        Just val -> return (Just val)
        Nothing -> return Nothing

iMI (IStmt stmt) = do
    iMS stmt

------------------------------------------ INSTRUCTIONS -------------------------------------------
-- Instructions include definitions and statements, so they can modify everything.
-- StmtResult is either a store or a store and a value (returned from a return statement).
-- StmtState is a pair of StmtResult and ControlFlow, which is used in loops.
iI :: Instr -> VEnv -> FEnv -> Store -> Either Error (VEnv, FEnv, StmtState)

{-
ISeq.      Instr ::= Instr1 ";" Instr; -- right associative
IDef.       Instr1 ::= Def;
IStmt.      Instr1 ::= Stmt;
ISpecStmt.   Instr1 ::= SpecStmt;
-}

-- Monadic versions of functions below:
-- if first instruction returns a value, we don't execute the second one.
-- Also, if any break or continue counter is greater than 0, we don't execute the second instruction.
iI (ISeq instr0 instr1) rhoV rhoF sto = do
    (rhoV', rhoF', (res, (breakCount, continueFlag))) <- iI instr0 rhoV rhoF sto
    case res of
        StoreAndValue sto' val -> return (rhoV', rhoF', (StoreAndValue sto' val, (breakCount, continueFlag)))
        StoreOnly sto' ->
            if breakCount > 0 || continueFlag then
                return (rhoV', rhoF', (StoreOnly sto', (breakCount, continueFlag)))
            else
                iI instr1 rhoV' rhoF' sto'

-- -- For now, we won't use monad transformers. TODO Use Monad Transformers.
-- iI (IDef def) rhoV rhoF sto = do
--     let decRes = iD def rhoV rhoF sto
--     case decRes of
--         Left err -> Left err
--         Right (rhoV', rhoF', sto') -> return (rhoV', rhoF', (StoreOnly sto', (0, False)))

iI (IStmt stmt) rhoV rhoF sto = do
    let stmtRes = iS stmt rhoV rhoF sto
    case stmtRes of
        Left err -> Left err
        Right (res, (breakCount, continueFlag)) -> return (rhoV, rhoF, (res, (breakCount, continueFlag)))


-- commented because the specstmt semantics has changed
-- iI (ISpecStmt specStmt) rhoV rhoF sto = do
--     let (rhoV', sto') = iSpecS specStmt rhoV rhoF sto
--     return (rhoV', rhoF, (StoreOnly sto', (0, False)))

-------------------------------VARIABLE DEFINITIONS (monadic)------------------------------------------------

iMD :: Def -> WorkingMonad (VEnv, FMEnv)

iMD (VarDef stype (Ident var) expr) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    val <- eMe expr
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV var (loc, (False, False))
    controlFlow <- getControlFlow
    put (setVarVal rhoV' sto' var (SimpleVal val), controlFlow)
    return (rhoV', rhoF)

iMD (ArrDefInit stype (Ident arr) exprSize exprInitVal) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    (VInt size) <- eMe exprSize
    if size < 0 then throwError (InvalidArraySize size)
    else do
        initExprRes <- eMe exprInitVal
        case initExprRes of
            VInt initVal -> do
                let (loc, sto') = newloc sto
                let arrVal = replicate (fromInteger size) (VInt initVal)
                let rhoV' = mapSet rhoV arr (loc, (False, False))
                controlFlow <- getControlFlow
                put (setVarVal rhoV' sto' arr (ComplexVal (VArray arrVal)), controlFlow)
                return (rhoV', rhoF)
            _ -> throwError (TypeMismatch SType FType)

iMD (ArrDef stype (Ident arr) exprSize) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    (VInt size) <- eMe exprSize
    if size < 0 then throwError (InvalidArraySize size)
    else do
        let (loc, sto') = newloc sto
        let arrVal = replicate (fromInteger size) (if stype == STInt then VInt 0 else VBool False)
        let rhoV' = mapSet rhoV arr (loc, (False, False))
        controlFlow <- getControlFlow
        put (setVarVal rhoV' sto' arr (ComplexVal (VArray arrVal)), controlFlow)
        return (rhoV', rhoF)

iMD (DictDef stype (Ident dict)) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV dict (loc, (False, False))
    controlFlow <- getControlFlow
    put (setVarVal rhoV' sto' dict (ComplexVal (VDict empty)), controlFlow)
    return (rhoV', rhoF)

iMD (FuncDef ftype (Ident func) params instr) = do
    (rhoV, rhoF) <- ask
    let x :: [FuncArg] -> WorkingMonad(SimpleValue) 
        x = \args -> do 
            let paramList = eP params
            (rhoV, rhoF) <- msetarguments (paramList) args
            let rhoF' = mapSet rhoF func x
            res <- local (const (rhoV, rhoF')) (iMI instr)
            case res of
                Just val -> return (val)
                Nothing -> return (VInt 0)
    let rhoF' = mapSet rhoF func x
    return (rhoV, rhoF')


--reszta todo (funkcje jako argumenty)

msetarguments :: [FuncParam] -> [FuncArg] -> WorkingMonad (VEnv, FMEnv)
msetarguments [] [] = do
    (rhoV, rhoF) <- ask
    return (rhoV, rhoF)

msetarguments (PSimple stype (Ident var) : restParams) (SimpleArg val : restArgs) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV var (loc, (False, False))
    putStore (setVarVal rhoV' sto' var (SimpleVal val))
    local (const (rhoV', rhoF)) $ do
        msetarguments restParams restArgs

msetarguments (PFunc ftype (Ident func) : restParams) (FArg f : restArgs) = do
    (rhoV, rhoF) <- ask
    let rhoF' = mapSet rhoF func f
    local (const (rhoV, rhoF')) $ do
        msetarguments restParams restArgs

-------------------------------VARIABLE DEFINITIONS------------------------------------------------
-- Definitions also can modify everything.
iD :: Def -> VEnv -> FEnv -> Store -> Either Error (VEnv, FEnv, Store)

{-
VarDef.        Def1 ::= SType Ident "=" Expr;

ArrDefInit.    Def1 ::= "Array" SType Ident "[" Expr "]" "(" Expr ")"; -- initialized with last argument
ArrDef.        Def1 ::= "Array" SType Ident "[" Expr "]";
DictDef.       Def1 ::= "Dict" SType Ident;

-- functions can also have local variables inside, so we allow declarations in the function body
FuncDef.       Def1 ::= FType Ident "(" Params ")" "{" Instr "}";
-}

-- Monad version of iD
iD (VarDef stype (Ident var) expr) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', val) -> do
            let (loc, sto'') = newloc sto'
            let rhoV' = mapSet rhoV var (loc, (False, False))
            return (rhoV', rhoF, setVarVal rhoV' sto'' var (SimpleVal val))

-- Arrays can be initialized with both boolean or integer values. Size must be a non-negative integer.
iD (ArrDefInit stype (Ident arr) exprSize exprInitVal) rhoV rhoF sto = do
    let sizeExprRes = eE exprSize rhoV rhoF sto
    case sizeExprRes of
        Left err -> Left err
        Right (sto', VInt size) -> do
            if size < 0 then Left (InvalidArraySize size)
            else do
                let initExprRes = eE exprInitVal rhoV rhoF sto'
                case initExprRes of
                    Left err -> Left err
                    Right (sto'', val) -> do
                        let (loc, sto''') = newloc sto''
                        let arrVal = replicate (fromInteger size) val
                        let rhoV' = mapSet rhoV arr (loc, (False, False))
                        return (rhoV', rhoF, setVarVal rhoV' sto''' arr (ComplexVal (VArray arrVal)))

-- Now it's a bit tricky, because we need to know the type of the array to create it.
-- Integer arrays are initialized with zeros, boolean arrays with False.
iD (ArrDef stype (Ident arr) exprSize) rhoV rhoF sto = do
    let sizeExprRes = eE exprSize rhoV rhoF sto
    case sizeExprRes of
        Left err -> Left err
        Right (sto', VInt size) -> do
            if size < 0 then Left (InvalidArraySize size)
            else do
                let (loc, sto'') = newloc sto'
                let arrVal = replicate (fromInteger size) (if stype == STInt then VInt 0 else VBool False)
                let rhoV' = mapSet rhoV arr (loc, (False, False))
                return (rhoV', rhoF, setVarVal rhoV' sto'' arr (ComplexVal (VArray arrVal)))

-- Dictionaries are empty by default.
iD (DictDef stype (Ident dict)) rhoV rhoF sto = do
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV dict (loc, (False, False))
    return (rhoV', rhoF, setVarVal rhoV' sto' dict (ComplexVal (VDict empty)))

iD (FuncDef ftype (Ident func) params instr) rhoV rhoF sto = do
    let x args sto' = do
        let paramList = eP params
        let (rhoV', rhoF', sto'') = prepareEnvs paramList rhoV rhoF sto'
        let (rhoV'', rhoF'', sto''') = assignArgs paramList args rhoV' rhoF' sto''
        let rhoF''' = mapSet rhoF'' func x
        let instrRes = iI instr rhoV'' rhoF''' sto'''
        case instrRes of
            Left err -> Left err
            Right (_, _, (StoreAndValue resSto resVal, _)) -> return (resSto, resVal)
    return (rhoV, mapSet rhoF func x, sto)

----------------------------------ARGUMENTS AND PARAMETERS (monadic) ----------------------------------------
eMa :: Args -> WorkingMonad [FuncArg]


eMa (ArgsVoid) = return []
eMa (ArgsOne expr) = do
    val <- eMe expr
    return [SimpleArg val]
eMa (ArgsMany expr args) = do
    val <- eMe expr
    vals <- eMa args
    return (SimpleArg val : vals)
eMa (ArgsLambda lambda) = do
    f <- eML lambda
    return [FArg f]
eMa (ArgsLambdaMany lambda args) = do
    f <- eML lambda
    fs <- eMa args
    return (FArg f : fs)



eML :: Lambda -> WorkingMonad MFunc

eML (Lam ftype params instr) = do
    (rhoV, rhoF) <- ask
    let x :: [FuncArg] -> WorkingMonad(SimpleValue) 
        x = \args -> do 
            let paramList = eP params
            (rhoV, rhoF) <- msetarguments (paramList) args
            res <- local (const (rhoV, rhoF)) (iMI instr)
            case res of
                Just val -> return (val)
                Nothing -> return (VInt 0)
    return x

eMP :: Params -> WorkingMonad [FuncParam]

eMP (ParamsNone) = return []
eMP (ParamVar stype (Ident var)) = return [PSimple stype (Ident var)]
eMP (ParamFunc ftype (Ident func)) = return [PFunc ftype (Ident func)]
eMP (ParamVarMany stype (Ident var) params) = do
    rest <- eMP params
    return ((PSimple stype (Ident var)) : rest)
eMP (ParamFuncMany ftype (Ident func) params) = do
    rest <- eMP params
    return ((PFunc ftype (Ident func)) : rest)


----------------------------------ARGUMENTS AND PARAMETERS ----------------------------------------
eA :: Args -> VEnv -> FEnv -> Store -> Either Error (Store, [FuncArg])

{-
ArgsVoid. Args ::= "void";
ArgsOne.  Args ::= Expr;
ArgsMany. Args ::= Expr "," Args;
ArgsLambda. Args ::= Lambda;
ArgsLambdaMany. Args ::= Lambda "," Args;
-}

-- Monadic versions
eA (ArgsVoid) rhoV rhoF sto = Right (sto, [])

eA (ArgsOne expr) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', val) -> return (sto', [SimpleArg val])

eA (ArgsMany expr args) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', val) -> do
            let argsRes = eA args rhoV rhoF sto'
            case argsRes of
                Left err -> Left err
                Right (sto'', args') -> return (sto'', (SimpleArg val) : args')

-- eA (ArgsLambda lambda) rhoV rhoF sto = Right (sto, [FArg (eL lambda rhoV rhoF sto)])

-- eA (ArgsLambdaMany lambda args) rhoV rhoF sto = do
--     let argsRes = eA args rhoV rhoF sto
--     case argsRes of
--         Left err -> Left err
--         Right (sto', args') -> do
--             return (sto', (FArg (eL lambda rhoV rhoF sto)) : args')

-- Lam. Lambda ::= FType "lambda" "(" Params ")" "->" "{" Instr "}";
-- Important! Lambda definition itself can not return an error.
eL :: Lambda -> VEnv -> FEnv -> Store -> Func

-- Monadic version of lambda:
eL (Lam ftype params instr) rhoV rhoF sto =
    x where
        x args sto' = do
            let paramList = eP params
            let (rhoV', rhoF', sto'') = prepareEnvs paramList rhoV rhoF sto'
            let (rhoV'', rhoF'', sto''') = assignArgs paramList args rhoV' rhoF' sto''
            let instrRes = iI instr rhoV'' rhoF'' sto'''
            case instrRes of
                Left err -> Left err
                Right (_, _, (StoreAndValue resSto resVal, _)) -> return (resSto, resVal)

-- Parameters
eP :: Params -> [FuncParam]

{-
ParamsNone.      Params ::= "none";
ParamVar.        Params ::= SType Ident;
-- big decision here, in parameters we don't say exactly how the function looks like,
-- we only specify what it returns.
ParamFunc.       Params ::= FType Ident;
ParamVarMany.    Params ::= SType Ident "," Params;
ParamFuncMany.   Params ::= FType Ident "," Params;
-}

eP (ParamsNone) = []
eP (ParamVar stype (Ident var)) = [PSimple stype (Ident var)]
eP (ParamFunc ftype (Ident func)) = [PFunc ftype (Ident func)]
eP (ParamVarMany stype (Ident var) params) = (PSimple stype (Ident var)) : eP params
eP (ParamFuncMany ftype (Ident func) params) = (PFunc ftype (Ident func)) : eP params

------------------------------------------ FUNCTION DEFINITIONS -----------------------------------
-- Now the most interesting part - functions. Functions can take simple values or other functions as arguments.
-- They can also have local variables in them and make recursive calls. They return simple values.
-- First we'll create a helper function to "prepare" the environments based on the parameters.
-- Then a function to assign the arguments to the parameters in new environments.
-- Then execute the function body.

-- FuncParam can be either a simple type or a function type.
prepareEnvs :: [FuncParam] -> VEnv -> FEnv -> Store -> (VEnv, FEnv, Store)

prepareEnvs [] rhoV rhoF sto = (rhoV, rhoF, sto)

prepareEnvs ((PSimple stype (Ident var)):params) rhoV rhoF sto =
    let (loc, sto') = newloc sto in
    let rhoV' = mapSet rhoV var (loc, (False, False)) in
        prepareEnvs params rhoV' rhoF sto'

-- Since lambda can be the only functional-argument in our language, we can create a dummy function
-- for now and assign it to the parameter.
prepareEnvs ((PFunc ftype (Ident func)):params) rhoV rhoF sto =
    let (loc, sto') = newloc sto in
    let rhoF' = mapSet rhoF func (\_ _ -> Right (sto', VInt 0)) in
        prepareEnvs params rhoV rhoF' sto'

-- We take each parameter and assign the corresponding argument to it. This function will be called
-- after prepareEnvs, so we can assume that the environments are already prepared.
-- NOTE: assignArgs does not modify the VEnv.
assignArgs :: [FuncParam] -> [FuncArg] -> VEnv -> FEnv -> Store -> (VEnv, FEnv, Store)

-- We iterate through the parameters and arguments at the same time, determine what type is the parameter,
-- find it's location in the environment and assign the argument to it.
assignArgs [] [] rhoV rhoF sto = (rhoV, rhoF, sto)

assignArgs ((PSimple stype (Ident var)):params) ((SimpleArg val):args) rhoV rhoF sto =
    let loc = mapGet rhoV var in
    let sto' = setVarVal rhoV sto var (SimpleVal val) in
        assignArgs params args rhoV rhoF sto'

-- We need to override the dummy function which we assigned before with the actual function in the argument.
-- assignArgs ((PFunc ftype (Ident func)):params) ((FArg f):args) rhoV rhoF sto =
--     let rhoF' = mapSet rhoF func f in
--         assignArgs params args rhoV rhoF' sto

------------------------------------------ STATEMENTS (monadic) ---------------------------------------------

iMS :: Stmt -> WorkingMonad (Maybe SimpleValue)

iMS (SSkip) = return Nothing

iMS (SBreak exp0) = do
    (VInt n) <- eMe exp0
    if n < 0 then throwError (InvalidBreakArgument n)
    else do
        putControlFlow (n, False) -- the flags had to be zero before the break
        return Nothing

iMS (SBreak1) = do
    putControlFlow (1, False)
    return Nothing

iMS (SContinue exp0) = do
    (VInt n) <- eMe exp0
    if n < 0 then throwError (InvalidContinueArgument n)
    else do
        putControlFlow (0, True)
        return Nothing

iMS (SContinue0) = do
    putControlFlow (0, True)
    return Nothing

iMS (SIf expr i0 i1) = do
    (VBool b) <- eMe expr
    if b then iMI i0
    else iMI i1

iMS (SWhile expr i) = do
    let x = do
        (VBool b) <- eMe expr
        if b then do
            res <- iMI i
            case res of
                Just val -> return (Just val)
                Nothing -> do
                    (breakCount, continueFlag) <- getControlFlow
                    if breakCount > 0 || continueFlag then
                        return Nothing
                    else
                        x
        else
            return Nothing
    x

iMS (SFor (Ident var) exprFrom exprTo instr) = do
    (VInt from) <- eMe exprFrom
    (VInt to) <- eMe exprTo
    (rhoV, rhoF) <- iMD (VarDef STInt (Ident var) (ENum from))
    local (const (rhoV, rhoF)) $ do
        let x = do
            (VInt val) <- eMe (VarVal (Ident var))
            if val <= to then do
                res <- iMI instr
                case res of
                    Just val -> return (Just val)
                    Nothing -> do
                        (breakCount, continueFlag) <- getControlFlow
                        if breakCount > 0 || continueFlag then
                            return Nothing
                        else do
                            msetVarVal var (SimpleVal (VInt (val + 1)))
                            x
            else
                return Nothing
        x

iMS (SReturn expr) = do
    val <- eMe expr
    return (Just val)

iMS (SPrint expr) = do
    val <- eMe expr
    liftIO $ print val
    return Nothing

iMS (VarAssign (Ident var) expr) = do
    val <- eMe expr
    msetVarVal var (SimpleVal val)
    return Nothing

iMS (VarAssignPlus (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))        -- todo: throw error if it is not int
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (n + m)))
            return (Nothing)
        _ -> throwError (TypeMismatch SType FType) --todo: write actural type 

iMS (VarAssignMinus (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (m - n)))
            return (Nothing)
        _ -> throwError (TypeMismatch SType FType) --todo: write actural type

iMS (VarAssignMul (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (n * m)))
            return (Nothing)
        _ -> throwError (TypeMismatch SType FType) --todo: write actural type

iMS (VarAssignDiv (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            if n == 0 then throwError DivByZero
            else do
                msetVarVal var (SimpleVal (VInt (m `div` n)))
                return (Nothing)
        _ -> throwError (TypeMismatch SType FType) --todo: write actural type

iMS (VarAssignMod (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            if n == 0 then throwError ModByZero
            else do
                msetVarVal var (SimpleVal (VInt (m `mod` n)))
                return (Nothing)
        _ -> throwError (TypeMismatch SType FType) --todo: write actural type


iMS (ArrElSet (Ident arr) exprIndex exprVal) = do
    val <- eMe exprVal
    (VInt index) <- eMe exprIndex
    a <- mgetarray (Ident arr)
    if index < 0 || (fromInteger index) >= (length a) then throwError (IndexOutOfBounds index)
    else do
        msetVarVal arr (ComplexVal (VArray (replaceNth a (fromInteger index) val)))
        return (Nothing)

iMS (DictElSet (Ident dict) exprKey exprVal) = do
    val <- eMe exprVal
    (VInt key) <- eMe exprKey
    d <- mgetdict (Ident dict)
    msetVarVal dict (ComplexVal (VDict (mapSet d key val)))
    return (Nothing)

------------------------------------------ STATEMENTS ---------------------------------------------
-- Statements do not modify the environment, but can modify the store. The return statement
-- can also return a value, so we take that into account in the function type.

iS :: Stmt -> VEnv -> FEnv -> Store -> Either Error StmtState

-- First, break and continue statements. They modify the control flow.
-- break n means break n nested loops, continue outer n means first perform n breaks, then continue.
{-
SBreak.     Stmt1 ::= "break" "(" Expr ")";
SBreak1.    Stmt1 ::= "break";
SContinue.  Stmt1 ::= "continue" "outer" "(" Expr ")";
SContinue0. Stmt1 ::= "continue";
-}

-- Monadic versions:

-- Statements should not get StoreAndValue as a argument, because the return statement
-- should quit the function immediately.
iS (SBreak exp0) rhoV rhoF sto = do
    let exprRes = eE exp0 rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt n) -> do
            if n < 0 then Left (InvalidBreakArgument n)
            else return (StoreOnly sto', (n, False))

iS (SBreak1) rhoV rhoF sto = Right (StoreOnly sto, (1, False))

iS (SContinue exp0) rhoV rhoF sto = do
    let exprRes = eE exp0 rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt n) -> do
            if n < 0 then Left (InvalidContinueArgument n)
            else return (StoreOnly sto', (0, True))

iS (SContinue0) rhoV rhoF sto = Right (StoreOnly sto, (0, True))

{-
internal SIf. Stmt1 ::= "if" "(" Expr ")" "{" Instr "}" "else" "{" Instr "}";
sif1.       Stmt1 ::= "if" "(" Expr ")" "{" Instr "}" "else" "{" Instr "}";
sif2.       Stmt1 ::= "if" "(" Expr ")" "{" Instr "}";
SWhile.     Stmt1 ::= "while" "(" Expr ")" "{" Instr "}";
SFor.       Stmt1 ::= "for" "(" Ident "=" Expr "to" Expr ")" "{" Instr "}";
-}

iS (SIf expr i0 i1) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VBool b) -> do
            if b then do
                let trueBranchRes = iI i0 rhoV rhoF sto'
                case trueBranchRes of
                    Left err -> Left err
                    Right (_, _, res) -> return res
            else do
                let falseBranchRes = iI i1 rhoV rhoF sto'
                case falseBranchRes of
                    Left err -> Left err
                    Right (_, _, res) -> return res

-- Monadic version of while
iS (SWhile expr i) rhoV rhoF sto = x rhoV rhoF sto where
    x rv rf st = do
        let exprRes = eE expr rv rf st
        case exprRes of
            Left err -> Left err
            Right (st', VBool b) -> do
                if b then
                    let instrRes = iI i rv rf st' in
                    case instrRes of
                        Left err -> Left err
                        Right (rv', rf', (res, (breakCount, continueFlag))) -> do
                            case res of
                                StoreAndValue st'' val -> return (StoreAndValue st'' val, (0, False))
                                StoreOnly st'' ->
                                    if breakCount > 0 then
                                        return (StoreOnly st'', (breakCount - 1, continueFlag))
                                    else if continueFlag then
                                        x rv' rf' st''
                                    else
                                        x rv' rf' st''
                else
                    return (StoreOnly st', (0, False))

-- Monadic version of for
iS (SFor (Ident var) exprFrom exprTo instr) rhoV rhoF sto = do
    let fromExprRes = eE exprFrom rhoV rhoF sto
    case fromExprRes of
        Left err -> Left err
        Right (sto', VInt from) -> do
            let toExprRes = eE exprTo rhoV rhoF sto'
            case toExprRes of
                Left err -> Left err
                Right (sto'', VInt to) -> do
                    let (loc, sto''') = newloc sto''
                    let rhoV' = mapSet rhoV var (loc, (False, False))
                    let sto'''' = setVarVal rhoV' sto''' var (SimpleVal (VInt from))

                    let x rv rf st = do
                        let SimpleVal (VInt i) = getVarVal rhoV' st var
                        if i <= to then
                            let instrRes = iI instr rv rf st in
                            case instrRes of
                                Left err -> Left err
                                Right (rv', rf', (res, (breakCount, continueFlag))) -> do
                                    case res of
                                        StoreAndValue sto' val -> return (StoreAndValue sto' val, (0, False))
                                        StoreOnly sto' ->
                                            if breakCount > 0 then
                                                return (StoreOnly sto', (breakCount - 1, continueFlag))
                                            else if continueFlag then
                                                let st' = setVarVal rhoV' sto' var (SimpleVal (VInt (i + 1))) in x rv' rf' st'
                                            else
                                                let st' = setVarVal rhoV' sto' var (SimpleVal (VInt (i + 1))) in x rv' rf' st'
                        else
                            return (StoreOnly st, (0, False))

                    x rhoV' rhoF sto'''

{-
SSkip.      Stmt1 ::= "skip";
SReturn.    Stmt1 ::= "return" "(" Expr ")";
SPrint.     Stmt1 ::= "print" "(" Expr ")";
SSwap.      Stmt1 ::= "swap" "(" Ident "," VarIdent ")";
SBreak.     Stmt1 ::= "break" "(" Expr ")";
SBreak1.    Stmt1 ::= "break";
SContinue.  Stmt1 ::= "continue" "outer" "(" Expr ")";
SContinue0. Stmt1 ::= "continue";
-}

iS (SSkip) rhoV rhoF sto = Right (StoreOnly sto, (0, False))

iS (SReturn expr) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', val) -> return (StoreAndValue sto' val, (0, False))

iS (SPrint expr) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', val) -> do
            unsafePrint ("Printing: " ++ show val) `seq` return (StoreOnly sto', (0, False))

-- Assigning variables.
-- NOTE: x += f(args) calculates x first, then f, so if f modifies x, it will be unseen.
{-
VarAssign.         Stmt1 ::= Ident "=" Expr;
VarAssignPlus.     Stmt1 ::= Ident "+=" Expr;
VarAssignMinus.    Stmt1 ::= Ident "-=" Expr;
VarAssignMul.      Stmt1 ::= Ident "*=" Expr;
VarAssignDiv.      Stmt1 ::= Ident "/=" Expr;
VarAssignMod.      Stmt1 ::= Ident "%=" Expr;

ArrElSet.      Stmt1 ::= Ident "[" Expr "]" "=" "(" Expr ")";
DictElSet.     Stmt1 ::= Ident "set" "[" Expr "]" "to" "(" Expr ")";
-}

-- Monadic versions:
iS (VarAssign (Ident var) expr) rhoV rhoF sto = do
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', val) -> do
            let sto'' = setVarVal rhoV sto' var (SimpleVal val)
            return (StoreOnly sto'', (0, False))

iS (VarAssignPlus (Ident var) expr) rhoV rhoF sto = do
    let SimpleVal (VInt x) = getVarVal rhoV sto var
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt y) -> do
            let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x + y)))
            return (StoreOnly sto'', (0, False))

iS (VarAssignMinus (Ident var) expr) rhoV rhoF sto = do
    let SimpleVal (VInt x) = getVarVal rhoV sto var
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt y) -> do
            let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x - y)))
            return (StoreOnly sto'', (0, False))

iS (VarAssignMul (Ident var) expr) rhoV rhoF sto = do
    let SimpleVal (VInt x) = getVarVal rhoV sto var
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt y) -> do
            let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x * y)))
            return (StoreOnly sto'', (0, False))

-- In the following two assignments, we need to handle division / modulo by zero.

iS (VarAssignDiv (Ident var) expr) rhoV rhoF sto = do
    let SimpleVal (VInt x) = getVarVal rhoV sto var
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt y) -> do
            if y == 0 then Left DivByZero
            else do
                let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x `div` y)))
                return (StoreOnly sto'', (0, False))

iS (VarAssignMod (Ident var) expr) rhoV rhoF sto = do
    let SimpleVal (VInt x) = getVarVal rhoV sto var
    let exprRes = eE expr rhoV rhoF sto
    case exprRes of
        Left err -> Left err
        Right (sto', VInt y) -> do
            if y == 0 then Left DivByZero
            else do
                let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x `mod` y)))
                return (StoreOnly sto'', (0, False))

-- Now monadic versions of array and dictionary assignments. Index out of bounds error can occur.

iS (ArrElSet (Ident arr) exprIndex exprVal) rhoV rhoF sto = do
    let ComplexVal (VArray a) = getVarVal rhoV sto arr
    let exprIndexRes = eE exprIndex rhoV rhoF sto
    case exprIndexRes of
        Left err -> Left err
        Right (sto', VInt i) -> do
            if i < 0 || i >= fromIntegral (length a) then Left (IndexOutOfBounds i)
            else do
                let exprValRes = eE exprVal rhoV rhoF sto'
                case exprValRes of
                    Left err -> Left err
                    Right (sto'', val) -> do
                        let a' = replaceNth a (fromInteger i) val
                        let sto''' = setVarVal rhoV sto'' arr (ComplexVal (VArray a'))
                        return (StoreOnly sto''', (0, False))

iS (DictElSet (Ident dict) exprIndex exprVal) rhoV rhoF sto = do
    let ComplexVal (VDict d) = getVarVal rhoV sto dict
    let exprIndexRes = eE exprIndex rhoV rhoF sto
    case exprIndexRes of
        Left err -> Left err
        Right (sto', VInt i) -> do
            let exprValRes = eE exprVal rhoV rhoF sto'
            case exprValRes of
                Left err -> Left err
                Right (sto'', val) -> do
                    let d' = insert i val d
                    let sto''' = setVarVal rhoV sto'' dict (ComplexVal (VDict d'))
                    return (StoreOnly sto''', (0, False))

-------------------------------------SPECIAL STATEMENTS (monadic) --------------------------------------------

mgetvarloc :: Ident -> WorkingMonad (Loc, DebugFlags)

mgetvarloc (Ident var) = do
    (rhoV, _) <- ask
    case Data.Map.lookup var rhoV of
        Just (loc, flags) -> return (loc, flags)
        Nothing -> throwError (VariableNotDefined (Ident var))

iMSpecS :: SpecStmt -> WorkingMonad (VEnv, FMEnv)


iMSpecS (SSwap (Ident var1) (Ident var2)) = do
    (rhoV, rhoF) <- ask
    (loc1, _) <- mgetvarloc (Ident var1)
    (loc2, _) <- mgetvarloc (Ident var2)
    let rhoV' = setVarLoc (setVarLoc rhoV var1 loc2) var2 loc1
    return (rhoV', rhoF)

iMSpecS (DebugAssEnable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (readFlag, _)) <- mgetvarloc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (readFlag, True))
    return (rhoV', rhoF)

iMSpecS (DebugAssDisable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (readFlag, _)) <- mgetvarloc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (readFlag, False))
    return (rhoV', rhoF)

iMSpecS (DebugReadEnable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (_, writeFlag)) <- mgetvarloc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (True, writeFlag))
    return (rhoV', rhoF)

iMSpecS (DebugReadDisable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (_, writeFlag)) <- mgetvarloc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (False, writeFlag))
    return (rhoV', rhoF)


-------------------------------------SPECIAL STATEMENTS--------------------------------------------
-- Special statements can modify the VEnv (setting debug flags) or the store (swapping variables).
-- There is no function swapping for now, so FEnv is not modified.
iSpecS :: SpecStmt -> VEnv -> FEnv -> Store -> (VEnv, Store)

{-
SSwap.              SpecStmt ::= "swap" "(" Ident "," Ident ")";
DebugAssEnable.     SpecStmt ::= "debug" "assignment" "enable" Ident;      -- ;)
DebugAssDisable.    SpecStmt ::= "debug" "assignment" "disable" Ident;
DebugReadEnable.    SpecStmt ::= "debug" "reading" "enable" Ident;
DebugReadDisable.   SpecStmt ::= "debug" "reading" "disable" Ident;
-}

-- Swapping does NOT swap the debugging flags. It only swaps the locations in the store.
-- It does not swap the values associated with the locations, it swaps locations themselves.
-- we can use setVarLoc to change the location of the variable.
iSpecS (SSwap (Ident var1) (Ident var2)) rhoV rhoF sto =
    let (loc1, _) = mapGet rhoV var1 in
    let (loc2, _) = mapGet rhoV var2 in
    let rhoV' = setVarLoc rhoV var1 loc2 in
    let rhoV'' = setVarLoc rhoV' var2 loc1 in
        (rhoV'', sto)

iSpecS (DebugAssEnable (Ident var)) rhoV rhoF sto =
    let (loc, (readFlag, _)) = mapGet rhoV var in
    let rhoV' = mapSet rhoV var (loc, (readFlag, True)) in
        (rhoV', sto)

iSpecS (DebugAssDisable (Ident var)) rhoV rhoF sto =
    let (loc, (readFlag, _)) = mapGet rhoV var in
    let rhoV' = mapSet rhoV var (loc, (readFlag, False)) in
        (rhoV', sto)

iSpecS (DebugReadEnable (Ident var)) rhoV rhoF sto =
    let (loc, (_, writeFlag)) = mapGet rhoV var in
    let rhoV' = mapSet rhoV var (loc, (True, writeFlag)) in
        (rhoV', sto)

iSpecS (DebugReadDisable (Ident var)) rhoV rhoF sto =
    let (loc, (_, writeFlag)) = mapGet rhoV var in
    let rhoV' = mapSet rhoV var (loc, (False, writeFlag)) in
        (rhoV', sto)




-- Example usage of the interpreter
main :: IO ()
main = do
    getContents >>= mcompute
    putStrLn ""

rhoF0:: FEnv
rhoF0 = fromList []

rhoFM0:: FMEnv
rhoFM0 = fromList []

rhoV0:: VEnv
rhoV0 = fromList []

sto0:: Store
sto0 = CStore empty 0

tsto0:: TypeStore
tsto0 = TStore empty 0

-- After computations we print the VEnv and the Store.
--compute s =
--    case pInstr (myLexer s) of
--        Left err -> do
--            putStrLn "\nParse              Failed...\n"
--            putStrLn err
--            exitFailure
--        Right e -> do
--            putStrLn "\nParse Successful!\n"
--            let instrRes = iI e rhoV0 rhoF0 sto0
--            case instrRes of
--                Left err -> do
--                    putStrLn "\nError during computation:"
--                    putStrLn $ show err
--                Right (rhoV', rhoF', (res, _)) -> do
--                    putStrLn "\nInterpretation     Successful!\n"
--                    rhoV' `seq` rhoF' `seq` res `seq` do
--                    putStrLn "\nEnd of computation"
--                    putStrLn "\nVEnv:"
--                    putStrLn $ show rhoV'
--                    putStrLn "\nStore:"
--                    putStrLn $ show res


mcompute :: String -> IO ()
mcompute s =
    case pInstr (myLexer s) of
        Left err -> do
            putStrLn "\nParse Failed...\n"
            putStrLn err
            exitFailure
        Right e -> do
            putStrLn "\nParse Successful!\n"

            let initialEnv = (rhoV0, rhoFM0)
            let initialState = (sto0, (0, False))

            -- For static type checking:
            -- let initialEnv = (rhoV0, rhoF0, TypeCheck)
            -- let initialState = (sto0, tsto0, (0, False))
--            typeCheckResult <- evalStateT (runReaderT (runExceptT (runWorkingMonad (iMI e))) initialEnv) initialState
--            case typeCheckResult of
--                Left err -> do
--                    putStrLn "\nType checking failed...\n"
--                    putStrLn $ "Error: " ++ show err
--                    exitFailure
--                Right _ -> do
--                    putStrLn "\nType checking successful!\n"


            result <- evalStateT (runReaderT (runExceptT (runWorkingMonad (iMI e))) initialEnv) initialState


            case result of
                Left err -> do
                    putStrLn "\nComputation Failed...\n"
                    putStrLn $ "Error: " ++ show err  -- Błąd wykonania (z `Left err`)
                    exitFailure
                Right _ -> do
                    putStrLn "\nEnd of computation\n"

                    putStrLn "\nVEnv:"
                    putStrLn $ show rhoV0

                    putStrLn "\nStore:"
                    putStrLn $ show sto0

-- Definicja funkcji
--processFile path = do
--    content <- readFile path
--    let strippedContent = Prelude.filter (/= '\n') content
--    compute strippedContent

mprocessFile path = do
    content <- readFile path
    let strippedContent = Prelude.filter (/= '\n') content
    mcompute strippedContent