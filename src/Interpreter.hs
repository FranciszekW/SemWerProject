-- TODOS:
-- > Static type checking

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Prelude

import System.IO (readFile, hFlush, stdout, stderr, hPutStrLn)
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
import FraJer.Abs   ( STInt(..), STBool(..), FTInt(..), FTBool(..), SimpleType(..), FuncType(..),
                      Ident(..), Expr(..), Args(..), Params(..),
                      Instr(..), Def(..), Stmt(..), SpecStmt(..), Lambda(..) )
import FraJer.Lex   ( Token, mkPosToken )
import FraJer.Par   ( pExpr, pInstr, pDef, pStmt, pLambda, myLexer )
import FraJer.Print ( Print, printTree )
--import FraJer.Skel  ()

mapGet :: (Ord k) => (Map k v) -> k -> v
mapGet map arg = map ! arg

mapSet :: (Ord k) => (Map k v) -> k -> v -> (Map k v)
mapSet map arg val = insert arg val map

mapHasKey :: (Ord k) => (Map k v) -> k -> Bool
mapHasKey map arg = member arg map


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
            | BreakUsageOutsideLoop
            | ContinueUsageOutsideLoop
            | NotASimpleValue Ident
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
    show BreakUsageOutsideLoop = "Break used outside of loop"
    show ContinueUsageOutsideLoop = "Continue used outside of loop"
    show (NotASimpleValue (Ident var)) = "Variable " ++ var ++ " is not a simple value"

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

data Store = CStore {currMap :: Map Loc Value, nextLoc :: Loc} deriving Show

data StmtResult = StoreOnly Store | StoreAndValue Store SimpleValue deriving Show
type StmtState = (StmtResult, ControlFlow)


------------------------------------------ MONADS -------------------------------------------------

type MFunc = [FuncArg] -> WorkingMonad (SimpleValue)
type FMEnv = Map Var MFunc

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

-- If the variable is not defined, we throw an error.
msetVarVal :: Var -> Value -> WorkingMonad ()
msetVarVal var val = do
    (rhoV, _) <- ask
    sto <- getStore
    case Data.Map.lookup var rhoV of
        Just (loc, (_, writeFlag)) -> do
            if writeFlag then do
                 liftIO $ putStrLn ("Writing: " ++ show var ++ " = " ++ show val)
                 putStore (CStore (mapSet (currMap sto) loc val) (nextLoc sto))
            else do
                 putStore (CStore (mapSet (currMap sto) loc val) (nextLoc sto))
        Nothing -> throwError (VariableNotDefined (Ident var))

-- Here we throw an error if the variable is not defined.
mgetVarVal :: Var -> WorkingMonad Value
mgetVarVal var = do
    (rhoV, _) <- ask
    sto <- getStore
    case Data.Map.lookup var rhoV of
        Just (loc, (readFlag, _)) -> do
            let val = mapGet (currMap sto) loc
            if readFlag then do
                liftIO $ putStrLn ("Reading: " ++ show var ++ " = " ++ show val)
                return val
            else do
                return val
        Nothing -> throwError (VariableNotDefined (Ident var))

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


replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs n newVal = Prelude.take n xs ++ [newVal] ++ Prelude.drop (n + 1) xs

----------------------------------------- EXPRESSIONS ------------------------------------------

eMe :: Expr -> WorkingMonad SimpleValue

eMe (ENum n) = return (VInt n)

eMe (FuncVal (Ident func) args) = do
    f <- mgetfunc (Ident func)
    arguments <- eMa args
    res <- f arguments
    return res

-- monadic semantics of VarVal using mgetVarVal
eMe (VarVal (Ident var)) = do
    val <- mgetVarVal var
    case val of
        SimpleVal x -> return x
        ComplexVal _ -> throwError (NotASimpleValue (Ident var))

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

eMe (ENeg exp0) = do
    (VInt x) <- eMe exp0
    return (VInt (-x))

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

------------------------------------------ INSTRUCTIONS ------------------------------------------
iMI :: Instr -> WorkingMonad (Maybe SimpleValue, VEnv, FMEnv)

-- Version with new return type
iMI (ISeq instr0 instr1) = do
    (res0, rhoV0, rhoF0) <- iMI instr0
    case res0 of
        Just val -> return (Just val, rhoV0, rhoF0)
        Nothing -> do
            (breakCount, continueFlag) <- getControlFlow
            if  (breakCount > 0 || continueFlag) then
                return (Nothing, rhoV0, rhoF0)
            else do
                local (const (rhoV0, rhoF0)) (iMI instr1)

iMI (IDef def) = do
    (rhoV, rhoF) <- iMD def
    return (Nothing, rhoV, rhoF) -- definitions don't return anything

iMI (IStmt stmt) = do
    (rhoV, rhoF) <- ask
    res <- iMS stmt
    case res of
        Just val -> return (Just val, rhoV, rhoF)
        Nothing -> return (Nothing, rhoV, rhoF)

iMI (ISpecStmt specStmt) = do
    (rhoV, rhoF) <- iMSpecS specStmt
    return (Nothing, rhoV, rhoF)


------------------------------------------ DEFINITIONS ------------------------------------------

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
            VBool initVal -> do
                let (loc, sto') = newloc sto
                let arrVal = replicate (fromInteger size) (VBool initVal)
                let rhoV' = mapSet rhoV arr (loc, (False, False))
                controlFlow <- getControlFlow
                put (setVarVal rhoV' sto' arr (ComplexVal (VArray arrVal)), controlFlow)
                return (rhoV', rhoF)

iMD (ArrDef stype (Ident arr) exprSize) = do
    let arrType = evalSimpleType stype
    (rhoV, rhoF) <- ask
    sto <- getStore
    (VInt size) <- eMe exprSize
    if size < 0 then throwError (InvalidArraySize size)
    else do
        let (loc, sto') = newloc sto
        let arrVal = replicate (fromInteger size) (if arrType == SimpleInt STInt then VInt 0 else VBool False)
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
            (res, _, _) <- local (const (rhoV, rhoF')) (iMI instr)
            case res of
                Just val -> return (val)
                Nothing -> case (evalFuncType ftype) of
                    FuncInt FTInt -> return (VInt 0)
                    FuncBool FTBool -> return (VBool False)
    let rhoF' = mapSet rhoF func x
    return (rhoV, rhoF')

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

---------------------------------- ARGUMENTS AND PARAMETERS ----------------------------------------

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
            (res, _, _) <- local (const (rhoV, rhoF)) (iMI instr)
            case res of
                Just val -> return (val)
                Nothing -> return (VInt 0)
    return x

eMP :: Params -> WorkingMonad [FuncParam]

eMP (ParamsNone) = return []
eMP (ParamVar stype (Ident var)) = return [PSimple (evalSimpleType stype) (Ident var)]
eMP (ParamFunc ftype (Ident func)) = return [PFunc (evalFuncType ftype) (Ident func)]
eMP (ParamVarMany stype (Ident var) params) = do
    rest <- eMP params
    return ((PSimple (evalSimpleType stype) (Ident var)) : rest)
eMP (ParamFuncMany ftype (Ident func) params) = do
    rest <- eMP params
    return ((PFunc (evalFuncType ftype) (Ident func)) : rest)


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
eP (ParamVar stype (Ident var)) = [PSimple (evalSimpleType stype) (Ident var)]
eP (ParamFunc ftype (Ident func)) = [PFunc (evalFuncType ftype) (Ident func)]
eP (ParamVarMany stype (Ident var) params) = (PSimple (evalSimpleType stype) (Ident var)) : eP params
eP (ParamFuncMany ftype (Ident func) params) = (PFunc (evalFuncType ftype) (Ident func)) : eP params

------------------------------------------ STATEMENTS ------------------------------------------

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
        putControlFlow (n, True)
        return Nothing

iMS (SContinue0) = do
    putControlFlow (0, True)
    return Nothing

iMS (SIf expr i0 i1) = do
    (VBool b) <- eMe expr
    if b then do
        (res1, _, _) <- iMI i0
        return res1
    else do
        (res2, _, _) <- iMI i1
        return res2

iMS (SWhile expr i) = do
    let x = do
        (VBool b) <- eMe expr
        if b then do
            (res, rhoV, rhoF) <- iMI i -- instructions in while loop can modify the environment
            case res of
                Just val -> do
                    putControlFlow (0, False) -- reset the flags
                    return (Just val)
                Nothing -> do
                    (breakCount, continueFlag) <- getControlFlow
                    if breakCount > 0 then do
                        putControlFlow (breakCount - 1, continueFlag)
                        return Nothing
                    else if continueFlag then do
                        putControlFlow (0, False) -- reset the flags
                        local (const (rhoV, rhoF)) x
                    else
                        local (const (rhoV, rhoF)) x
        else do
            putControlFlow (0, False) -- reset the flags
            return Nothing
    x

iMS (SFor (Ident var) exprFrom exprTo instr) = do
    (VInt from) <- eMe exprFrom
    (VInt to) <- eMe exprTo
    (rhoV, rhoF) <- iMD (VarDef (STI STInt) (Ident var) (ENum from))
    local (const (rhoV, rhoF)) $ do
        let x = do
            (VInt val) <- eMe (VarVal (Ident var))
            if val <= to then do
                (res, rhoV, rhoF) <- iMI instr
                case res of
                    Just val -> do
                        putControlFlow (0, False) -- reset the flags
                        return (Just val)
                    Nothing -> do
                        (breakCount, continueFlag) <- getControlFlow
                        if breakCount > 0 then do
                            putControlFlow (breakCount - 1, continueFlag)
                            return Nothing
                        else if continueFlag then do
                            putControlFlow (0, False) -- reset the flags
                            msetVarVal var (SimpleVal (VInt (val + 1)))
                            local (const (rhoV, rhoF)) x
                        else do
                            msetVarVal var (SimpleVal (VInt (val + 1)))
                            local (const (rhoV, rhoF)) x
            else do
                putControlFlow (0, False) -- reset the flags
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
--        _ -> throwError (TypeMismatch (TSimple STInt) (typeof val)) -- static type checker will catch this

iMS (VarAssignMinus (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (m - n)))
            return (Nothing)
--        _ -> throwError (TypeMismatch SType FType) --static type checker should catch this

iMS (VarAssignMul (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (n * m)))
            return (Nothing)
--        _ -> throwError (TypeMismatch SType FType) static type checker should catch this

iMS (VarAssignDiv (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            if n == 0 then throwError DivByZero
            else do
                msetVarVal var (SimpleVal (VInt (m `div` n)))
                return (Nothing)
--        _ -> throwError (TypeMismatch SType FType) --static type checker should catch this

iMS (VarAssignMod (Ident var) expr) = do
    val <- eMe expr
    (VInt m) <- eMe (VarVal (Ident var))
    case val of
        VInt n -> do
            if n == 0 then throwError ModByZero
            else do
                msetVarVal var (SimpleVal (VInt (m `mod` n)))
                return (Nothing)
--        _ -> throwError (TypeMismatch SType FType) --todo: write actural type


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


------------------------------------------ SPECIAL STATEMENTS ------------------------------------------

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



-- Example usage of the interpreter
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> mprocessFile filePath
        _ -> putStrLn "Usage: interpreter <file>"


rhoFM0:: FMEnv
rhoFM0 = fromList []

rhoV0:: VEnv
rhoV0 = fromList []

sto0:: Store
sto0 = CStore empty 0

tsto0:: TypeStore
tsto0 = TStore empty 0


mcompute :: String -> IO ()
mcompute s =
    case pInstr (myLexer s) of
        Left err -> do
            putStrLn "Parse Failed...\n"
            putStrLn err
            exitFailure
        Right e -> do
            putStrLn "Parse Successful!\n"

            let initialEnv = (rhoV0, rhoFM0)
            let initialState = (sto0, (0, False))

            result <- evalStateT (runReaderT (runExceptT (runWorkingMonad (iMI e))) initialEnv) initialState

            case result of
                Left err -> do
                    putStrLn "Computation Failed..." >> hFlush stdout
                    hPutStrLn stderr $ "Error: " ++ show err  -- Błąd wykonania (z `Left err`)
                    exitFailure
                Right res -> do
                    case res of
                        (Just val, _, _) -> do
                            putStrLn "Computation Successful!" >> hFlush stdout
                            putStrLn $ "Program returned with value: " ++ show val
                        (Nothing, _, _) -> do
                            putStrLn "Computation Successful!\n" >> hFlush stdout
                            putStrLn "Program returned without a value."


mprocessFile :: FilePath -> IO ()
mprocessFile path = do
    content <- readFile path
    let strippedContent = Prelude.filter (/= '\n') content
    mcompute strippedContent

--STInt.  STInt ::= "Int";
--STBool. STBool ::= "Bool";
--FTInt. FTInt ::= "IntFunc";
--FTBool. FTBool ::= "BoolFunc";
--
--STI. SimpleType ::= STInt;
--STB. SimpleType ::= STBool;
--FTI. FuncType ::= FTInt;
--FTB. FuncType ::= FTBool;
evalSimpleType :: SimpleType -> SType
evalSimpleType (STI STInt) = SimpleInt STInt
evalSimpleType (STB STBool) = SimpleBool STBool

evalFuncType :: FuncType -> FType
evalFuncType (FTI FTInt) = FuncInt FTInt
evalFuncType (FTB FTBool) = FuncBool FTBool

------------------------------------------ TYPE CHECKING -------------------------------------------
-- Static type checker

data SType = SimpleInt STInt | SimpleBool STBool deriving (Show, Eq)
data FType = FuncInt FTInt | FuncBool FTBool deriving (Show, Eq)

-- SType and FType are types from Frajer.cf.
data Type = TSimple SType | TFunc FType | TComplex ComplexType | TFunction DetailedFuncType deriving (Show, Eq)

-- Complex types (arrays and dictionaries)
data ComplexType = TArray SType | TDict SType deriving (Show, Eq)

data FuncParam = PSimple SType Ident | PFunc FType Ident deriving (Show, Eq)

-- Function types (can take functions as arguments, but return only simple types)
data DetailedFuncType = DetFunc [FuncParam] SType deriving (Show, Eq)

typeof :: SimpleValue -> Type
typeof (VInt _) = TSimple (SimpleInt STInt)
typeof (VBool _) = TSimple (SimpleBool STBool)

type TVEnv = Map Var Loc -- the same as VEnv, but for clarity called TEnv
type TFEnv = Map Var DetailedFuncType
-- A flag to detect whether we are in a loop and break/continue should be allowed
type LoopFlag = Bool
data TypeStore = TStore {typeMap :: Map Loc Type, nextTypeLoc :: Loc} deriving Show

tnewloc:: TypeStore -> (Loc, TypeStore)
tnewloc (TStore map loc) = (loc, TStore map (loc + 1))

tsetVarLoc:: TVEnv -> Var -> Loc -> TVEnv
tsetVarLoc trhoV var loc = mapSet trhoV var loc

------------------------------------------ MONAD -------------------------------------------------
--newtype WorkingMonad a = WorkingMonad { runWorkingMonad :: ExceptT Error (ReaderT (VEnv, FMEnv) (StateT (Store, ControlFlow) IO)) a }
-- Now our monad for working with types
newtype TypeMonad a = TypeMonad { runTypeMonad :: ExceptT Error (ReaderT (TVEnv, TFEnv) (StateT (TypeStore, LoopFlag) IO)) a }

instance MonadState (TypeStore, LoopFlag) TypeMonad where
    get = TypeMonad $ lift get
    put = TypeMonad . lift . put

instance MonadError Error TypeMonad where
    throwError = TypeMonad . throwError
    catchError (TypeMonad action) handler =
        TypeMonad $ catchError action (runTypeMonad . handler)

instance MonadReader (TVEnv, TFEnv) TypeMonad where
    ask = TypeMonad ask
    local f (TypeMonad action) = TypeMonad $ local f action

instance Functor TypeMonad where
    fmap = liftM

instance Applicative TypeMonad where
    pure = return
    (<*>) = ap

instance Monad TypeMonad where
    return = TypeMonad . return
    (TypeMonad action) >>= f = TypeMonad (action >>= runTypeMonad . f)

instance MonadIO TypeMonad where
    liftIO = TypeMonad . liftIO

instance MonadFail TypeMonad where
    fail msg = throwError (CustomError (msg))


------------------------------------------ HELPER FUNCTIONS ---------------------------------------

getTStore :: TypeMonad TypeStore
getTStore = TypeMonad $ do
    (tsto, _) <- get
    return tsto

putTStore :: TypeStore -> TypeMonad ()
putTStore tsto = do
    (_, loopFlag) <- get
    put (tsto, loopFlag)

getLoopFlag :: TypeMonad LoopFlag
getLoopFlag = TypeMonad $ do
    (_, loopFlag) <- get
    return loopFlag

putLoopFlag :: LoopFlag -> TypeMonad ()
putLoopFlag loopFlag = do
    (tsto, _) <- get
    put (tsto, loopFlag)

getVarType :: Var -> TypeMonad Type
getVarType var = do
    (trhoV, _) <- ask
    tsto <- getTStore
    case Data.Map.lookup var trhoV of
        Just loc -> case Data.Map.lookup loc (typeMap tsto) of
            Just t -> return t
            Nothing -> throwError (VariableNotDefined (Ident var))
        Nothing -> throwError (VariableNotDefined (Ident var))

setVarType :: Var -> Type -> TypeMonad ()
setVarType var t = do
    (trhoV, _) <- ask
    tsto <- getTStore
    case Data.Map.lookup var trhoV of
        Just loc -> do
            let tsto' = tsto {typeMap = mapSet (typeMap tsto) loc t}
            putTStore tsto'
        Nothing -> throwError (VariableNotDefined (Ident var))


-------------------------------------- EXPRESSIONS ------------------------------------------------
-- Now type checking functions, similarly to semantic functions, we create separate ones
-- for different syntactic categories

checkExpr :: Expr -> TypeMonad Type

checkExpr (ENum _) = return (TSimple (SimpleInt STInt))
