
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Executor (
    runProgram
) where

import Prelude

import Control.Monad      ( ap, liftM )
import Control.Monad.Reader ( ReaderT, MonadReader, MonadIO, runReaderT, ask, local, liftIO, lift )
import Control.Monad.State  ( StateT, MonadState, evalStateT, get, put )
import Control.Monad.Except ( ExceptT, MonadError, runExceptT, throwError, catchError )
import Control.Monad.Identity ( )

import TypeChecker

import Data.Map

-- Syntactic categories given in the FraJer.cf file
import FraJer.Abs   ( SSTInt(..), SSTBool(..), SimpleType(..),
                      Ident(..), Expr(..), Args(..), Params(..),
                      Instr(..), Def(..), Stmt(..), SpecStmt(..), Lambda(..) )

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
            | TooLargeBreakArgument Integer
            | InvalidContinueArgument Integer
            | TooLargeContinueArgument Integer
            -- | TypeMismatch Type Type
            | VariableNotDefined Ident
            | BreakUsageOutsideLoop
            | ContinueUsageOutsideLoop
            | NotASimpleValue Ident
            -- | InvalidPrintArgType Type
            -- | InvalidSwapArgType Type
            -- | InvalidDebugArgType Type
            | CustomError String

instance Show Error where
    show DivByZero = "Division by zero"
    show ModByZero = "Modulo by zero"
    show (KeyNotInDict k) = "Key " ++ show k ++ " not in dictionary"
    show (FunctionNotInScope (Ident f)) = "Function " ++ f ++ " not in scope"
    show (IndexOutOfBounds i) = "Index " ++ show i ++ " out of bounds"
    show (InvalidArraySize s) = "Invalid array size: " ++ show s
    show (InvalidBreakArgument n) = "Invalid break argument: " ++ show n
    show (TooLargeBreakArgument n) = "Too large break argument: " ++ show n
    show (InvalidContinueArgument n) = "Invalid continue argument: " ++ show n
    show (TooLargeContinueArgument n) = "Too large continue argument: " ++ show n
    -- show (TypeMismatch t1 t2) = "Type mismatch: " ++ show t1 ++ " and " ++ show t2
    show (VariableNotDefined (Ident var)) = "Variable " ++ var ++ " not defined"
    show BreakUsageOutsideLoop = "Break used outside of loop"
    show ContinueUsageOutsideLoop = "Continue used outside of loop"
    show (NotASimpleValue (Ident var)) = "Variable " ++ var ++ " is not a simple value"
    -- show (InvalidPrintArgType t) = "Invalid print argument type: " ++ show t
    -- show (InvalidSwapArgType t) = "Invalid swap argument type: " ++ show t
    -- show (InvalidDebugArgType t) = "Invalid debug argument type: " ++ show t

------------------------------------------ ENVIRONMENTS -------------------------------------------

type Var = String
type Loc = Integer
type DebugFlag = Bool
type BreakCount = Integer
-- ContinueFlag is simply a bool, because continue outer n is equivalent to n breaks and then continue.
type ContinueFlag = Bool
type NestingLevel = Integer

type ControlFlow = (BreakCount, ContinueFlag, NestingLevel)

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
    (breakCount, _, _) <- runWorkingMonad getControlFlow
    return breakCount

getContinueFlag :: WorkingMonad Bool
getContinueFlag = WorkingMonad $ do
    (_, continueFlag, _) <- runWorkingMonad getControlFlow
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

-- In the non-monadic version, we don't have to check any flags. This function will be used
-- only to declare variables, in which case the debugging flags are always false.
setVarVal:: VEnv -> Store -> Var -> Value -> Store
setVarVal rhoV sto var val =
    let (loc, (_, writeFlag)) = mapGet rhoV var in
    let map0 = mapSet (currMap sto) loc val in
        CStore map0 (nextLoc sto)

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

newloc:: Store -> (Loc, Store)
newloc (CStore map loc) = (loc, CStore map (loc + 1))

setVarLoc:: VEnv -> Var -> Loc -> VEnv
setVarLoc rhoV var loc =
    let (_, debugFlags) = mapGet rhoV var in
        mapSet rhoV var (loc, debugFlags)

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
    ComplexVal (VDict d) <- mgetVarVal dict
    return (VBool (mapHasKey d i))

-- helper functions:

monadicEvalBinaryIntOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
monadicEvalBinaryIntOp op exp0 exp1 = do
    valX <- eMe exp0
    valY <- eMe exp1
    case (valX, valY) of
        (VInt x, VInt y) -> return (VBool (x `op` y))
        -- _ -> throwError $ TypeMismatch (typeof (valX)) (typeof (valY))

monadicEvalBinaryBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
monadicEvalBinaryBoolOp op exp0 exp1 = do
    valX <- eMe exp0
    valY <- eMe exp1
    case (valX, valY) of
        (VBool x, VBool y) -> return (VBool (x `op` y))
        -- _ -> throwError $ TypeMismatch (typeof valX) (typeof valY)


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
            ComplexVal (VArray a) <- mgetVarVal arr
            return a
        Nothing -> throwError (VariableNotDefined (Ident arr))

mgetdict :: Ident -> WorkingMonad Dict
mgetdict (Ident dict) = do
    (rhoV, _) <- ask
    case Data.Map.lookup dict rhoV of
        Just (loc, _) -> do
            sto <- getStore
            ComplexVal (VDict d) <- mgetVarVal dict
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
            (breakCount, continueFlag, _) <- getControlFlow
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
                Nothing -> case (evalFuncReturnType ftype) of
                    SimpleInt STInt -> return (VInt 0)
                    SimpleBool STBool -> return (VBool False)
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
eMa (ArgsFunc func) = do
    f <- mgetfunc func
    return [FArg f]
eMa (ArgsFuncMany func args) = do
    f <- mgetfunc func
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

--eMP :: Params -> WorkingMonad [FuncParam]
--
--eMP (ParamsNone) = return []
--eMP (ParamVar stype (Ident var)) = return [PSimple (evalSimpleType stype) (Ident var)]
--eMP (ParamVarMany stype (Ident var) params) = do
--    rest <- eMP params
--    return ((PSimple (evalSimpleType stype) (Ident var)) : rest)
--eMP (ParamFunc ftype params (Ident func)) = do
--    funcParams <- eMP params
--    paramTypes <- evalParamTypes funcParams
--    returnType <- evalFuncReturnType ftype
--    return [PFunc (DetFunc paramTypes returnType) (Ident func)]
--eMP (ParamFuncMany ftype params (Ident func) paramsMany) = do
--    funcParams <- eMP params
--    paramTypes <- evalParamTypes funcParams
--    rest <- eMP paramsMany
--    returnType <- evalFuncReturnType ftype
--    return ((PFunc (DetFunc paramTypes returnType) (Ident func)) : rest)


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
eP (ParamVarMany stype (Ident var) params) = (PSimple (evalSimpleType stype) (Ident var)) : eP params
eP (ParamFunc ftype params (Ident func)) =
    let funcParams = eP params in
    let paramTypes = evalParamTypes funcParams in
    [PFunc (DetFunc paramTypes (evalFuncReturnType ftype)) (Ident func)]
eP (ParamFuncMany ftype params (Ident func) paramsMany) =
    let funcParams = eP params in
    let paramTypes = evalParamTypes funcParams in
    let rest = eP paramsMany in
    [PFunc (DetFunc paramTypes (evalFuncReturnType ftype)) (Ident func)] ++ rest

------------------------------------------ STATEMENTS ------------------------------------------

iMS :: Stmt -> WorkingMonad (Maybe SimpleValue)

iMS (SSkip) = return Nothing

iMS (SBreak exp0) = do
    (VInt n) <- eMe exp0
    (_, _, nestingLevel) <- getControlFlow
    if n < 0 then throwError (InvalidBreakArgument n)
    else do
        if nestingLevel < n then throwError (TooLargeBreakArgument n)
        else do
            putControlFlow (n, False, nestingLevel)
            return Nothing

iMS (SBreak1) = do
    (_, _, nestingLevel) <- getControlFlow
    if nestingLevel < 1 then throwError (TooLargeBreakArgument 1)
    else do
        putControlFlow (1, False, nestingLevel)
        return Nothing

iMS (SContinue exp0) = do
    (VInt n) <- eMe exp0
    (_, _, nestingLevel) <- getControlFlow
    if n < 0 then throwError (InvalidContinueArgument n)
    else do
        if nestingLevel < n then throwError (TooLargeContinueArgument n)
        else do
            putControlFlow (n, True, nestingLevel)
            return Nothing

iMS (SContinue0) = do
    (_, _, nestingLevel) <- getControlFlow
    if nestingLevel < 1 then throwError (TooLargeContinueArgument 1)
    else do
        putControlFlow (0, True, nestingLevel)
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
    (bc, cf, nl) <- getControlFlow
    putControlFlow (bc, cf, nl + 1)
    let x = do
        (VBool b) <- eMe expr
        if b then do
            (res, rhoV, rhoF) <- iMI i -- instructions in while loop can modify the environment
            case res of
                Just val -> do
                    putControlFlow (0, False, 0) -- reset the flags
                    return (Just val)
                Nothing -> do
                    (breakCount, continueFlag, _) <- getControlFlow
                    if breakCount > 0 then do
                        putControlFlow (breakCount - 1, continueFlag, nl)
                        return Nothing
                    else if continueFlag then do
                        putControlFlow (0, False, nl + 1) -- reset the flags
                        local (const (rhoV, rhoF)) x
                    else
                        local (const (rhoV, rhoF)) x
        else do
            putControlFlow (0, False, nl) -- reset the flags
            return Nothing
    x

iMS (SFor (Ident var) exprFrom exprTo instr) = do
    (VInt from) <- eMe exprFrom
    (VInt to) <- eMe exprTo
    (rhoV, rhoF) <- iMD (VarDef (STI STInt) (Ident var) (ENum from))
    (bc, cf, nl) <- getControlFlow
    putControlFlow (bc, cf, nl + 1)
    local (const (rhoV, rhoF)) $ do
        let x = do
            (VInt val) <- eMe (VarVal (Ident var))
            if val <= to then do
                (res, rhoV, rhoF) <- iMI instr
                case res of
                    Just val -> do
                        putControlFlow (0, False, 0) -- reset the flags
                        return (Just val)
                    Nothing -> do
                        (breakCount, continueFlag, _) <- getControlFlow
                        if breakCount > 0 then do
                            putControlFlow (breakCount - 1, continueFlag, nl)
                            return Nothing
                        else if continueFlag then do
                            putControlFlow (0, False, nl + 1) -- reset the flags
                            msetVarVal var (SimpleVal (VInt (val + 1)))
                            local (const (rhoV, rhoF)) x
                        else do
                            msetVarVal var (SimpleVal (VInt (val + 1)))
                            local (const (rhoV, rhoF)) x
            else do
                putControlFlow (0, False, nl) -- reset the flags
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

iMS (VarInc (Ident var)) = iMS (VarAssignPlus (Ident var) (ENum 1))

iMS (VarDec (Ident var)) = iMS (VarAssignMinus (Ident var) (ENum 1))

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

rhoFM0:: FMEnv
rhoFM0 = fromList []

rhoV0:: VEnv
rhoV0 = fromList []

initialEnv:: (VEnv, FMEnv)
initialEnv = (rhoV0, rhoFM0)

sto0:: Store
sto0 = CStore empty 0

initialState = (sto0, (0, False, 0))

runProgram :: Instr -> IO (Either Error (Maybe SimpleValue))
runProgram instr = do
    res <- evalStateT (runReaderT (runExceptT (runWorkingMonad (iMI instr))) initialEnv) initialState
    case res of
        Left err -> return (Left err)
        Right (val, _, _) -> return (Right val)