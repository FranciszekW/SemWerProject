-- TODOS:
-- > Remove unsafePrint from setVarVal and getVarVal
-- > Move static type checker to a separate file
-- > Tests to reconsider:
-- > good ()
-- > bad (103)

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
import FraJer.Abs   ( SSTInt(..), SSTBool(..), FFTInt(..), FFTBool(..), SimpleType(..), FuncType(..),
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
            | TooLargeBreakArgument Integer
            | InvalidContinueArgument Integer
            | TooLargeContinueArgument Integer
            | TypeMismatch Type Type
            | VariableNotDefined Ident
            | BreakUsageOutsideLoop
            | ContinueUsageOutsideLoop
            | NotASimpleValue Ident
            | InvalidPrintArgType Type
            | InvalidSwapArgType Type
            | InvalidDebugArgType Type
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
    show (TypeMismatch t1 t2) = "Type mismatch: " ++ show t1 ++ " and " ++ show t2
    show (VariableNotDefined (Ident var)) = "Variable " ++ var ++ " not defined"
    show BreakUsageOutsideLoop = "Break used outside of loop"
    show ContinueUsageOutsideLoop = "Continue used outside of loop"
    show (NotASimpleValue (Ident var)) = "Variable " ++ var ++ " is not a simple value"
    show (InvalidPrintArgType t) = "Invalid print argument type: " ++ show t
    show (InvalidSwapArgType t) = "Invalid swap argument type: " ++ show t
    show (InvalidDebugArgType t) = "Invalid debug argument type: " ++ show t

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

rhoFT0:: TFEnv
rhoFT0 = fromList []

rhoVT0:: TVEnv
rhoVT0 = fromList []

tsto0:: TypeStore
tsto0 = TStore empty 0


mcompute :: String -> IO ()
mcompute s =
    case pInstr (myLexer s) of
        Left err -> do
            putStrLn "Parse Failed...\n"
            hPutStrLn stderr err
            exitFailure
        Right e -> do
            putStrLn "Parse Successful!\n"

            let typeCheckEnv = (rhoVT0, rhoFT0)
            let typeCheckState = (tsto0, False)

            typeCheckResult <- evalStateT (runReaderT (runExceptT (runTypeMonad (checkInstr e))) typeCheckEnv) typeCheckState

            case typeCheckResult of
                Left err -> do
                    putStrLn "Type Check Failed..." >> hFlush stdout
                    hPutStrLn stderr $ "Error: " ++ show err  -- Błąd wykonania (z `Left err`)
                    exitFailure
                Right _ -> do
                    putStrLn "Type Check Successful!\n" >> hFlush stdout
                    let initialEnv = (rhoV0, rhoFM0)
                    let initialState = (sto0, (0, False, 0))

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


------------------------------------------ TYPE CHECKING -------------------------------------------
--data Error =  DivByZero
--            | ModByZero
--            | KeyNotInDict DictKey
--            | FunctionNotInScope Ident
--            | IndexOutOfBounds Integer
--            | InvalidArraySize Integer
--            | InvalidBreakArgument Integer
--            | InvalidContinueArgument Integer
--            | TypeMismatch Type Type
--            | VariableNotDefined Ident
--            | BreakUsageOutsideLoop
--            | ContinueUsageOutsideLoop
--            | NotASimpleValue Ident
--            | CustomError String
-- Static type checker

data SType = SimpleInt SSTInt | SimpleBool SSTBool deriving (Show, Eq)
--data FType = FuncInt FTInt | FuncBool FTBool deriving (Show, Eq)

-- SType and FType are types from Frajer.cf.
data Type = TSimple SType | TComplex ComplexType | TFunction DetailedFuncType deriving (Show, Eq)

-- Complex types (arrays and dictionaries)
data ComplexType = TArray SType | TDict SType deriving (Show, Eq)

data FuncParam = PSimple SType Ident | PFunc DetailedFuncType Ident deriving (Show, Eq)

-- Function types (can take functions as arguments, but return only simple types)
data DetailedFuncType = DetFunc [Type] SType deriving (Show, Eq)

typeof :: SimpleValue -> Type
typeof (VInt _) = TSimple (SimpleInt STInt)
typeof (VBool _) = TSimple (SimpleBool STBool)

type TVEnv = Map Var Loc -- the same as VEnv, but for clarity called TEnv
type TFEnv = Map Var DetailedFuncType
-- A flag to detect whether we are in a loop and break/continue should be allowed
type LoopFlag = Bool
data TypeStore = TStore {typeMap :: Map Loc Type, nextTypeLoc :: Loc} deriving Show

------------------------------------HELPER FUNCTIONS------------------------------------------------

tnewloc:: TypeStore -> (Loc, TypeStore)
tnewloc (TStore map loc) = (loc, TStore map (loc + 1))

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

evalFuncReturnType :: FuncType -> SType
evalFuncReturnType (FTI FTInt) = SimpleInt STInt
evalFuncReturnType (FTB FTBool) = SimpleBool STBool

evalParamTypes :: [FuncParam] -> [Type]
evalParamTypes [] = []
evalParamTypes (PSimple stype (Ident var) : rest) =
    TSimple (stype) : evalParamTypes rest
evalParamTypes (PFunc (DetFunc paramTypes retType) (Ident func) : rest) =
    TFunction (DetFunc paramTypes retType) : evalParamTypes rest

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

setLocType :: TypeStore -> Loc -> Type -> TypeStore
setLocType tsto loc t = tsto {typeMap = mapSet (typeMap tsto) loc t}

getLoopFlag :: TypeMonad LoopFlag
getLoopFlag = TypeMonad $ do
    (_, loopFlag) <- get
    return loopFlag

putLoopFlag :: LoopFlag -> TypeMonad ()
putLoopFlag loopFlag = do
    (tsto, _) <- get
    put (tsto, loopFlag)

tsetVarLoc :: TVEnv -> Var -> Loc -> TVEnv
tsetVarLoc rho var loc = mapSet rho var loc

getVarLoc :: Var -> TypeMonad Loc
getVarLoc var = do
    (rhoVT, _) <- ask
    case Data.Map.lookup var rhoVT of
        Just loc -> return loc
        Nothing -> throwError (VariableNotDefined (Ident var))

getVarType :: Var -> TypeMonad Type
getVarType var = do
    (rhoVT, _) <- ask
    tsto <- getTStore
    loc <- getVarLoc var
    case Data.Map.lookup loc (typeMap tsto) of
        Just t -> return t
        Nothing -> throwError (VariableNotDefined (Ident var))

setVarType :: Var -> Type -> TypeMonad ()
setVarType var t = do
    (rhoVT, _) <- ask
    tsto <- getTStore
    case Data.Map.lookup var rhoVT of
        Just loc -> do
            let tsto' = tsto {typeMap = mapSet (typeMap tsto) loc t}
            putTStore tsto'
        Nothing -> throwError (VariableNotDefined (Ident var))

getFuncType :: Var -> TypeMonad Type
getFuncType var = do
    (_, rhoFT) <- ask
    case Data.Map.lookup var rhoFT of
        Just t -> return (TFunction t)
        Nothing -> throwError (FunctionNotInScope (Ident var))

setFuncType :: TFEnv -> Var -> DetailedFuncType -> TFEnv
setFuncType rho func t = mapSet rho func t

checkAllTypesEqual :: [Type] -> Type -> TypeMonad ()
checkAllTypesEqual [] _ = return ()
checkAllTypesEqual (t:ts) t' = do
    if t == t' then checkAllTypesEqual ts t'
    else throwError (TypeMismatch t' t)

-------------------------------------- EXPRESSIONS ------------------------------------------------
-- Now type checking functions, similarly to semantic functions, we create separate ones
-- for different syntactic categories

checkExpr :: Expr -> TypeMonad Type

checkExpr (ENum _) = return (TSimple (SimpleInt STInt))

checkExpr (FuncVal (Ident func) args) = do
    t <- getFuncType func
    case t of
        TFunction (DetFunc paramTypes retType) -> do
            argTypes <- checkArgs args
            if argTypes == paramTypes then return (TSimple retType)
            else throwError (TypeMismatch (TFunction (DetFunc paramTypes retType)) (TFunction (DetFunc argTypes retType)))
        _ -> throwError (FunctionNotInScope (Ident func))

checkExpr (VarVal (Ident var)) = getVarType var

checkExpr (EPlus exp0 exp1) = checkBinaryIntOp exp0 exp1
checkExpr (EMinus exp0 exp1) = checkBinaryIntOp exp0 exp1
checkExpr (EDiv exp0 exp1) = checkBinaryIntOp exp0 exp1
checkExpr (EMul exp0 exp1) = checkBinaryIntOp exp0 exp1
checkExpr (EMod exp0 exp1) = checkBinaryIntOp exp0 exp1

checkExpr (ENeg exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleInt STInt) -> return (TSimple (SimpleInt STInt))
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkExpr (EArray (Ident arr) exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleInt STInt) -> do
            t' <- getVarType arr
            case t' of
                TComplex (TArray st) -> return (TSimple st)
                _ -> throwError (TypeMismatch (TComplex (TArray (SimpleInt STInt))) t')
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkExpr (EDict (Ident dict) exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleInt STInt) -> do
            t' <- getVarType dict
            case t' of
                TComplex (TDict st) -> return (TSimple st)
                _ -> throwError (TypeMismatch (TComplex (TDict (SimpleInt STInt))) t')
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkExpr (EPostInc (Ident var)) = checkIncDec var
checkExpr (EPreInc (Ident var)) = checkIncDec var
checkExpr (EPostDec (Ident var)) = checkIncDec var
checkExpr (EPreDec (Ident var)) = checkIncDec var

checkExpr (EEq exp0 exp1) = checkBinarySimplevalOp exp0 exp1
checkExpr (ENeq exp0 exp1) = checkBinarySimplevalOp exp0 exp1

checkExpr (ELt exp0 exp1) = checkBoolBinaryIntOp exp0 exp1
checkExpr (EGt exp0 exp1) = checkBoolBinaryIntOp exp0 exp1
checkExpr (EGeq exp0 exp1) = checkBoolBinaryIntOp exp0 exp1
checkExpr (ELeq exp0 exp1) = checkBoolBinaryIntOp exp0 exp1

checkExpr (BTrue) = return (TSimple (SimpleBool STBool))
checkExpr (BFalse) = return (TSimple (SimpleBool STBool))

checkExpr (BNot exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleBool STBool) -> return (TSimple (SimpleBool STBool))
        _ -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t)

checkExpr (BOr exp0 exp1) = checkBinaryBoolOp exp0 exp1
checkExpr (BAnd exp0 exp1) = checkBinaryBoolOp exp0 exp1
checkExpr (BXor exp0 exp1) = checkBinaryBoolOp exp0 exp1

checkExpr (BDictHasKey (Ident dict) exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleInt STInt) -> do
            t' <- getVarType dict
            case t' of
                TComplex (TDict _) -> return (TSimple (SimpleBool STBool))
                _ -> throwError (TypeMismatch (TComplex (TDict (SimpleInt STInt))) t')
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

-- helper functions:

checkBinaryIntOp :: Expr -> Expr -> TypeMonad Type
checkBinaryIntOp exp0 exp1 = do
    t0 <- checkExpr exp0
    t1 <- checkExpr exp1
    case (t0, t1) of
        (TSimple (SimpleInt STInt), TSimple (SimpleInt STInt)) -> return (TSimple (SimpleInt STInt))
        (TSimple (SimpleInt STInt), _) -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t1)
        -- When two types are not equal, we return the first one, as it is the one that caused the error
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t0)

checkBinarySimplevalOp :: Expr -> Expr -> TypeMonad Type
checkBinarySimplevalOp exp0 exp1 = do
    t0 <- checkExpr exp0
    t1 <- checkExpr exp1
    case (t0, t1) of
        (TSimple s0, TSimple s1) -> if s0 == s1 then return (TSimple (SimpleBool STBool))
            else throwError (TypeMismatch (TSimple s0) (TSimple s1))
        (TSimple _, _) -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t1)
        _ -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t0)

checkBoolBinaryIntOp :: Expr -> Expr -> TypeMonad Type
checkBoolBinaryIntOp exp0 exp1 = do
    t0 <- checkExpr exp0
    t1 <- checkExpr exp1
    case (t0, t1) of
        (TSimple (SimpleInt STInt), TSimple (SimpleInt STInt)) -> return (TSimple (SimpleBool STBool))
        (TSimple (SimpleInt STInt), _) -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t1)
        _ -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t0)

checkBinaryBoolOp :: Expr -> Expr -> TypeMonad Type
checkBinaryBoolOp exp0 exp1 = do
    t0 <- checkExpr exp0
    t1 <- checkExpr exp1
    case (t0, t1) of
        (TSimple (SimpleBool STBool), TSimple (SimpleBool STBool)) -> return (TSimple (SimpleBool STBool))
        (TSimple (SimpleBool STBool), _) -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t1)
        _ -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t0)

checkIncDec :: Var -> TypeMonad Type
checkIncDec var = do
    t <- getVarType var
    case t of
        TSimple (SimpleInt STInt) -> return (TSimple (SimpleInt STInt))
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

-------------------------------------- ARGUMENTS AND PARAMETERS ---------------------------------------

checkArgs :: Args -> TypeMonad [Type]

checkArgs (ArgsVoid) = return []

checkArgs (ArgsOne expr) = do
    t <- checkExpr expr
    return [t]

checkArgs (ArgsMany expr args) = do
    t <- checkExpr expr
    ts <- checkArgs args
    return (t : ts)

checkArgs (ArgsLambda lambda) = do
    t <- checkLambda lambda
    return [t]

checkArgs (ArgsLambdaMany lambda args) = do
    t <- checkLambda lambda
    ts <- checkArgs args
    return (t : ts)

checkArgs (ArgsFunc (Ident func)) = do
    t <- getFuncType func
    return [t]

checkArgs (ArgsFuncMany (Ident func) args) = do
    t <- getFuncType func
    ts <- checkArgs args
    return (t : ts)


checkLambda :: Lambda -> TypeMonad Type

checkLambda (Lam ftype params instr) = do
    let paramList = eP params
    let paramTypes = evalParamTypes paramList
    let retType = evalFuncReturnType ftype
    let funcType = DetFunc paramTypes retType
    return (TFunction funcType)

------------------------------------------ INSTRUCTIONS ------------------------------------------
-- We will return the list of types and this represents all the possible types returned
-- by the sequence of instructions. That is necessary for the type checker to keep track
-- of all possible types that can be returned by a function and check if they are consistent
-- and equal to the declared return type.
checkInstr :: Instr -> TypeMonad ([Type], TVEnv, TFEnv)

-- In the sequence of instructions, we need to check type of the first one, update the
-- environment (the first instruction could define a variable). It's a bit different from
-- the semantic function, here we need to check types of both instructions, even if the first
-- one returns a value.
checkInstr (ISeq i0 i1) = do
    (res0, rhoVT0, rhoFT0) <- checkInstr i0
    local (const (rhoVT0, rhoFT0)) $ do
        (res1, rhoVT1, rhoFT1) <- checkInstr i1
        return (res0 ++ res1, rhoVT1, rhoFT1)

checkInstr (IDef def) = do
    (rhoVT, rhoFT) <- checkDef def
    return ([], rhoVT, rhoFT)

checkInstr (IStmt stmt) = do
    res <- checkStmt stmt
    (rhoVT, rhoFT) <- ask
    return (res, rhoVT, rhoFT)

checkInstr (ISpecStmt specStmt) = do
    (rhoVT, rhoFT) <- checkSpecStmt specStmt
    return ([], rhoVT, rhoFT)

------------------------------------------ DEFINITIONS ------------------------------------------

checkDef :: Def -> TypeMonad (TVEnv, TFEnv)

checkDef (VarDef stype (Ident var) expr) = do
    t <- checkExpr expr
    case t of
        TSimple st -> if st == evalSimpleType stype then do
            (rhoVT, rhoFT) <- ask
            tsto <- getTStore
            let (loc, tsto') = tnewloc tsto
            let rhoVT' = mapSet rhoVT var loc
            let tsto'' = setLocType tsto' loc t
            putTStore tsto''
            return (rhoVT', rhoFT)
        else throwError (TypeMismatch (TSimple (evalSimpleType stype)) t)

checkDef (ArrDefInit stype (Ident arr) exprSize exprInitVal) = do
    t <- checkExpr exprSize
    case t of
        TSimple (SimpleInt STInt) -> do
            t' <- checkExpr exprInitVal
            case t' of
                TSimple st -> if st == evalSimpleType stype then do
                    (rhoVT, rhoFT) <- ask
                    tsto <- getTStore
                    let (loc, tsto') = tnewloc tsto
                    let rhoVT' = mapSet rhoVT arr loc
                    let tsto'' = setLocType tsto' loc (TComplex (TArray st))
                    putTStore tsto''
                    return (rhoVT', rhoFT)
                else throwError (TypeMismatch (TSimple (evalSimpleType stype)) t')
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkDef (ArrDef stype (Ident arr) exprSize) = do
    t <- checkExpr exprSize
    case t of
        TSimple (SimpleInt STInt) -> do
            (rhoVT, rhoFT) <- ask
            tsto <- getTStore
            let (loc, tsto') = tnewloc tsto
            let rhoVT' = mapSet rhoVT arr loc
            let tsto'' = setLocType tsto' loc (TComplex (TArray (evalSimpleType stype)))
            putTStore tsto''
            return (rhoVT', rhoFT)
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkDef (DictDef stype (Ident dict)) = do
    (rhoVT, rhoFT) <- ask
    tsto <- getTStore
    let (loc, tsto') = tnewloc tsto
    let rhoVT' = mapSet rhoVT dict loc
    let tsto'' = setLocType tsto' loc (TComplex (TDict (evalSimpleType stype)))
    putTStore tsto''
    return (rhoVT', rhoFT)

-- In functions, we check all possible return types from the instructions
-- and compare them with the function return type. We also need to update the environments
-- based on the function parameters.
checkDef (FuncDef ftype (Ident func) params instr) = do
    (rhoVT0, rhoFT0) <- ask
    let paramList = eP params
    let funcType = DetFunc (evalParamTypes paramList) (evalFuncReturnType ftype)
    (rhoVT, rhoFT) <- prepareParamEnv paramList
    let rhoFT' = setFuncType rhoFT func funcType
    local (const (rhoVT, rhoFT')) $ do
        (res, rhoVT', rhoFT'') <- checkInstr instr
        let retTypes = res
        let retType = evalFuncReturnType ftype
        checkAllTypesEqual retTypes (TSimple retType)
        return (rhoVT0, setFuncType rhoFT0 func funcType)


prepareParamEnv :: [FuncParam] -> TypeMonad (TVEnv, TFEnv)
prepareParamEnv [] = do
    (rhoVT, rhoFT) <- ask
    return (rhoVT, rhoFT)

prepareParamEnv (PSimple stype (Ident var) : rest) = do
    (rhoVT, rhoFT) <- prepareParamEnv rest
    tsto <- getTStore
    let (loc, tsto') = tnewloc tsto
    let rhoVT' = mapSet rhoVT var loc
    let tsto'' = setLocType tsto' loc (TSimple stype)
    putTStore tsto''
    return (rhoVT', rhoFT)

prepareParamEnv (PFunc (DetFunc paramTypes retType) (Ident func) : rest) = do
    (rhoVT, rhoFT) <- prepareParamEnv rest
    let rhoFT' = mapSet rhoFT func (DetFunc paramTypes retType)
    return (rhoVT, rhoFT')

------------------------------------------ STATEMENTS -------------------------------------------
-- In statements, we also hold the list of possible return types, i.e. if we have the if statement
-- with return in both branches, we need to check if the types are equal. If we have a break or
-- continue statement, we need to check if it is inside a loop.
checkStmt :: Stmt -> TypeMonad [Type]

checkStmt (SSkip) = return []

checkStmt (SBreak exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleInt STInt) -> do
            loopFlag <- getLoopFlag
            if loopFlag then return []
            else throwError BreakUsageOutsideLoop
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkStmt (SBreak1) = do
    loopFlag <- getLoopFlag
    if loopFlag then return []
    else throwError BreakUsageOutsideLoop

checkStmt (SContinue exp0) = do
    t <- checkExpr exp0
    case t of
        TSimple (SimpleInt STInt) -> do
            loopFlag <- getLoopFlag
            if loopFlag then return []
            else throwError ContinueUsageOutsideLoop
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

checkStmt (SContinue0) = do
    loopFlag <- getLoopFlag
    if loopFlag then return []
    else throwError ContinueUsageOutsideLoop

checkStmt (SIf expr i0 i1) = do
    t <- checkExpr expr
    case t of
        TSimple (SimpleBool STBool) -> do
            (res0, rhoVT0, rhoFT0) <- checkInstr i0
            (res1, rhoVT1, rhoFT1) <- checkInstr i1
            return (res0 ++ res1)
        _ -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t)

checkStmt (SWhile expr i) = do
    t <- checkExpr expr
    case t of
        TSimple (SimpleBool STBool) -> do
            loopFlag <- getLoopFlag
            putLoopFlag True
            (res, rhoVT, rhoFT) <- checkInstr i
            putLoopFlag loopFlag
            return res
        _ -> throwError (TypeMismatch (TSimple (SimpleBool STBool)) t)

--checkStmt (SFor (Ident var) exprFrom exprTo instr) = do
--    tfrom <- checkExpr exprFrom
--    case tfrom of
--        TSimple (SimpleInt STInt) -> do
--            tto <- checkExpr exprTo
--            case tto of
--                TSimple (SimpleInt STInt) -> do
--                    loopFlag <- getLoopFlag
--                    putLoopFlag True
--                    (res, rhoVT, rhoFT) <- checkInstr instr
--                    putLoopFlag loopFlag
--                    return res
--                _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) tto)
--
--        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) tfrom)

checkStmt (SFor (Ident var) exprFrom exprTo instr) = do
    tfrom <- checkExpr exprFrom
    case tfrom of
        TSimple (SimpleInt STInt) -> do
            tto <- checkExpr exprTo
            case tto of
                TSimple (SimpleInt STInt) -> do
                    loopFlag <- getLoopFlag
                    putLoopFlag True
                    (rhoVT, rhoFT) <- ask
                    tsto <- getTStore
                    let (loc, tsto') = tnewloc tsto
                    let rhoVT' = mapSet rhoVT var loc
                    let tsto'' = setLocType tsto' loc (TSimple (SimpleInt STInt))
                    putTStore tsto''
                    local (const (rhoVT', rhoFT)) $ do
                        (res, rhoVT, rhoFT) <- checkInstr instr
                        putLoopFlag loopFlag
                        return res
                _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) tto)

        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) tfrom)


checkStmt (SReturn expr) = do
    t <- checkExpr expr
    return [t]

-- Print is allowed only for simple types
checkStmt (SPrint expr) = do
    t <- checkExpr expr
    case t of
        TSimple _ -> return []
        _ -> throwError (InvalidPrintArgType t)

checkStmt (VarAssign (Ident var) expr) = do
    t <- checkExpr expr
    t' <- getVarType var
    if t == t' then return []
    else throwError (TypeMismatch t' t)

checkStmt (VarAssignPlus (Ident var) expr) = checkIntAssign var expr

checkStmt (VarAssignMinus (Ident var) expr) = checkIntAssign var expr

checkStmt (VarAssignMul (Ident var) expr) = checkIntAssign var expr

checkStmt (VarAssignDiv (Ident var) expr) = checkIntAssign var expr

checkStmt (VarAssignMod (Ident var) expr) = checkIntAssign var expr

checkStmt (VarInc (Ident var)) = checkIntAssign var (ENum 1)

checkStmt (VarDec (Ident var)) = checkIntAssign var (ENum 1)

checkStmt (ArrElSet (Ident arr) exprIndex exprVal) = do
    tIndex <- checkExpr exprIndex
    case tIndex of
        TSimple (SimpleInt STInt) -> do
            tVal <- checkExpr exprVal
            tArr <- getVarType arr
            case tArr of
                TComplex (TArray st) -> if tVal == TSimple st then return []
                    else throwError (TypeMismatch (TSimple st) tVal)
                _ -> throwError (TypeMismatch (TComplex (TArray (SimpleInt STInt))) tArr)
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) tIndex)

checkStmt (DictElSet (Ident dict) exprKey exprVal) = do
    tKey <- checkExpr exprKey
    case tKey of
        TSimple (SimpleInt STInt) -> do
            tVal <- checkExpr exprVal
            tDict <- getVarType dict
            case tDict of
                TComplex (TDict st) -> if tVal == TSimple st then return []
                    else throwError (TypeMismatch (TSimple st) tVal)
                _ -> throwError (TypeMismatch (TComplex (TDict (SimpleInt STInt))) tDict)
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) tKey)

checkIntAssign :: Var -> Expr -> TypeMonad [Type]
checkIntAssign var expr = do
    t <- checkExpr expr
    case t of
        TSimple (SimpleInt STInt) -> do
            t' <- getVarType var
            if t' == TSimple (SimpleInt STInt) then return []
            else throwError (TypeMismatch (TSimple (SimpleInt STInt)) t')
        _ -> throwError (TypeMismatch (TSimple (SimpleInt STInt)) t)

------------------------------------------ SPECIAL STATEMENTS -------------------------------------

checkSpecStmt :: SpecStmt -> TypeMonad (TVEnv, TFEnv)

-- Swapping variables is allowed only for simple types, arrays and dictionaries
checkSpecStmt (SSwap (Ident var1) (Ident var2)) = do
    (rhoVT, rhoFT) <- ask
    tsto <- getTStore
    t1 <- getVarType var1
    t2 <- getVarType var2
    case (t1, t2) of
        (TSimple (SimpleInt STInt), TSimple (SimpleInt STInt)) -> do
            loc1 <- getVarLoc var1
            loc2 <- getVarLoc var2
            let rhoVT' = tsetVarLoc (tsetVarLoc rhoVT var1 loc2) var2 loc1
            return (rhoVT', rhoFT)
        (TSimple (SimpleBool STBool), TSimple (SimpleBool STBool)) -> do
            loc1 <- getVarLoc var1
            loc2 <- getVarLoc var2
            let rhoVT' = tsetVarLoc (tsetVarLoc rhoVT var1 loc2) var2 loc1
            return (rhoVT', rhoFT)
        (TComplex (TArray st1), TComplex (TArray st2)) -> do
            if st1 == st2 then do
                loc1 <- getVarLoc var1
                loc2 <- getVarLoc var2
                let rhoVT' = tsetVarLoc (tsetVarLoc rhoVT var1 loc2) var2 loc1
                return (rhoVT', rhoFT)
            else throwError (TypeMismatch (TComplex (TArray st1)) (TComplex (TArray st2)))
        (TComplex (TDict st1), TComplex (TDict st2)) -> do
            if st1 == st2 then do
                loc1 <- getVarLoc var1
                loc2 <- getVarLoc var2
                let rhoVT' = tsetVarLoc (tsetVarLoc rhoVT var1 loc2) var2 loc1
                return (rhoVT', rhoFT)
            else throwError (TypeMismatch (TComplex (TDict st1)) (TComplex (TDict st2)))
        (TFunction _, _) -> throwError (InvalidSwapArgType t1)
        (_, TFunction _) -> throwError (InvalidSwapArgType t2)
        _ -> throwError (TypeMismatch t1 t2)

-- Debugging works only on Simple types
checkSpecStmt (DebugAssEnable (Ident var)) = checkDebug var

checkSpecStmt (DebugAssDisable (Ident var)) = checkDebug var

checkSpecStmt (DebugReadEnable (Ident var)) = checkDebug var

checkSpecStmt (DebugReadDisable (Ident var)) = checkDebug var

checkDebug :: Var -> TypeMonad (TVEnv, TFEnv)
checkDebug var = do
    t <- getVarType var
    case t of
        TSimple _ -> do
            (rhoVT, rhoFT) <- ask
            return (rhoVT, rhoFT)
        _ -> throwError (InvalidDebugArgType t)