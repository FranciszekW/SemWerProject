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
            | IndexOutOfBounds Integer
            | InvalidArraySize Integer
            | InvalidBreakArgument Integer
            | TooLargeBreakArgument Integer
            | InvalidContinueArgument Integer
            | TooLargeContinueArgument Integer
            | CustomError String

instance Show Error where
    show DivByZero = "Division by zero"
    show ModByZero = "Modulo by zero"
    show (KeyNotInDict k) = "Key " ++ show k ++ " not in dictionary"
    show (IndexOutOfBounds i) = "Index " ++ show i ++ " out of bounds"
    show (InvalidArraySize s) = "Invalid array size: " ++ show s
    show (InvalidBreakArgument n) = "Invalid break argument: " ++ show n
    show (TooLargeBreakArgument n) = "Too large break argument: " ++ show n
    show (InvalidContinueArgument n) = "Invalid continue argument: " ++ show n
    show (TooLargeContinueArgument n) = "Too large continue argument: " ++ show n

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

evalExpr :: Expr -> WorkingMonad SimpleValue

evalExpr (ENum n) = return (VInt n)

evalExpr (FuncVal (Ident func) args) = do
    f <- getFunc (Ident func)
    arguments <- evalArguments args
    res <- f arguments
    return res

-- monadic semantics of VarVal using mgetVarVal
evalExpr (VarVal (Ident var)) = do
    val <- mgetVarVal var
    case val of
        SimpleVal x -> return x

evalExpr (EPlus exp0 exp1) = do
    (VInt x) <- evalExpr exp0
    (VInt y) <- evalExpr exp1
    return (VInt (x + y))

evalExpr (EMinus exp0 exp1) = do
    (VInt x) <- evalExpr exp0
    (VInt y) <- evalExpr exp1
    return (VInt (x - y))

evalExpr (EDiv exp0 exp1) = do
    (VInt x) <- evalExpr exp0
    (VInt y) <- evalExpr exp1
    if y == 0 then throwError DivByZero
    else return (VInt (x `div` y))

evalExpr (EMul exp0 exp1) = do
    (VInt x) <- evalExpr exp0
    (VInt y) <- evalExpr exp1
    return (VInt (x * y))

evalExpr (EMod exp0 exp1) = do
    (VInt x) <- evalExpr exp0
    (VInt y) <- evalExpr exp1
    if y == 0 then throwError ModByZero
    else return (VInt (x `mod` y))

evalExpr (ENeg exp0) = do
    (VInt x) <- evalExpr exp0
    return (VInt (-x))

evalExpr (EArray (Ident arr) exp0) = do
    (VInt i) <- evalExpr exp0
    a <- getArray (Ident arr)
    if i < 0 || i >= toInteger (length a) then throwError (IndexOutOfBounds i)
    else return (a !! fromInteger i)

evalExpr (EDict (Ident dict) exp0) = do
    (VInt i) <- evalExpr exp0
    d <- getDict (Ident dict)
    if not (mapHasKey d i) then throwError (KeyNotInDict i)
    else return (d ! i)

evalExpr (EPostInc (Ident var)) = do
    (VInt x) <- evalExpr (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x + 1)))
    return (VInt x)

evalExpr (EPreInc (Ident var)) = do
    (VInt x) <- evalExpr (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x + 1)))
    return (VInt (x + 1))

evalExpr (EPostDec (Ident var)) = do
    (VInt x) <- evalExpr (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x - 1)))
    return (VInt x)

evalExpr (EPreDec (Ident var)) = do
    (VInt x) <- evalExpr (VarVal (Ident var))
    msetVarVal var (SimpleVal (VInt (x - 1)))
    return (VInt (x - 1))

--boools:
evalExpr (BTrue) = return (VBool True)
evalExpr (BFalse) = return (VBool False)

evalExpr (EEq exp0 exp1) = evalBinarySimpleValOp (==) exp0 exp1
evalExpr (ENeq exp0 exp1) = evalBinarySimpleValOp (/=) exp0 exp1
evalExpr (ELt exp0 exp1) = evalBinaryIntOp (<) exp0 exp1
evalExpr (EGt exp0 exp1) = evalBinaryIntOp (>) exp0 exp1
evalExpr (ELeq exp0 exp1) = evalBinaryIntOp (<=) exp0 exp1
evalExpr (EGeq exp0 exp1) = evalBinaryIntOp (>=) exp0 exp1

evalExpr (BNot exp0) = do
    (VBool x) <- evalExpr exp0
    return (VBool (not x))

evalExpr (BOr exp0 exp1) = evalBinaryBoolOp (||) exp0 exp1
evalExpr (BAnd exp0 exp1) = evalBinaryBoolOp (&&) exp0 exp1
evalExpr (BXor exp0 exp1) = evalBinaryBoolOp (/=) exp0 exp1

evalExpr (BDictHasKey (Ident dict) exp0) = do
    (VInt i) <- evalExpr exp0
    (rhoV, _) <- ask
    sto <- getStore
    ComplexVal (VDict d) <- mgetVarVal dict
    return (VBool (mapHasKey d i))

-- helper functions:

evalBinaryIntOp :: (Integer -> Integer -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
evalBinaryIntOp op exp0 exp1 = do
    valX <- evalExpr exp0
    valY <- evalExpr exp1
    case (valX, valY) of
        (VInt x, VInt y) -> return (VBool (x `op` y))
        -- _ -> throwError $ TypeMismatch (typeof (valX)) (typeof (valY))

evalBinaryBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
evalBinaryBoolOp op exp0 exp1 = do
    valX <- evalExpr exp0
    valY <- evalExpr exp1
    case (valX, valY) of
        (VBool x, VBool y) -> return (VBool (x `op` y))
        -- _ -> throwError $ TypeMismatch (typeof valX) (typeof valY)


evalBinarySimpleValOp :: (SimpleValue -> SimpleValue -> Bool) -> Expr -> Expr -> WorkingMonad SimpleValue
evalBinarySimpleValOp op exp0 exp1 = do
    x <- evalExpr exp0
    y <- evalExpr exp1
    return (VBool (x `op` y))

getFunc :: Ident -> WorkingMonad MFunc
getFunc (Ident func) = do
    (_, rhoF) <- ask
    case Data.Map.lookup func rhoF of
        Just f -> return f

getArray :: Ident -> WorkingMonad Arr
getArray (Ident arr) = do
    (rhoV, _) <- ask
    case Data.Map.lookup arr rhoV of
        Just (loc, _) -> do
            sto <- getStore
            ComplexVal (VArray a) <- mgetVarVal arr
            return a

getDict :: Ident -> WorkingMonad Dict
getDict (Ident dict) = do
    (rhoV, _) <- ask
    case Data.Map.lookup dict rhoV of
        Just (loc, _) -> do
            sto <- getStore
            ComplexVal (VDict d) <- mgetVarVal dict
            return d

------------------------------------------ INSTRUCTIONS ------------------------------------------
evalInstr :: Instr -> WorkingMonad (Maybe SimpleValue, VEnv, FMEnv)

-- Version with new return type
evalInstr (ISeq instr0 instr1) = do
    (res0, rhoV0, rhoF0) <- evalInstr instr0
    case res0 of
        Just val -> return (Just val, rhoV0, rhoF0)
        Nothing -> do
            (breakCount, continueFlag, _) <- getControlFlow
            if  (breakCount > 0 || continueFlag) then
                return (Nothing, rhoV0, rhoF0)
            else do
                local (const (rhoV0, rhoF0)) (evalInstr instr1)

evalInstr (IDef def) = do
    (rhoV, rhoF) <- evalDef def
    return (Nothing, rhoV, rhoF) -- definitions don't return anything

evalInstr (IStmt stmt) = do
    (rhoV, rhoF) <- ask
    res <- evalStmt stmt
    case res of
        Just val -> return (Just val, rhoV, rhoF)
        Nothing -> return (Nothing, rhoV, rhoF)

evalInstr (ISpecStmt specStmt) = do
    (rhoV, rhoF) <- evalSpecStmt specStmt
    return (Nothing, rhoV, rhoF)


------------------------------------------ DEFINITIONS ------------------------------------------

evalDef :: Def -> WorkingMonad (VEnv, FMEnv)

evalDef (VarDef stype (Ident var) expr) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    val <- evalExpr expr
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV var (loc, (False, False))
    controlFlow <- getControlFlow
    put (setVarVal rhoV' sto' var (SimpleVal val), controlFlow)
    return (rhoV', rhoF)

evalDef (ArrDefInit stype (Ident arr) exprSize exprInitVal) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    (VInt size) <- evalExpr exprSize
    if size < 0 then throwError (InvalidArraySize size)
    else do
        initExprRes <- evalExpr exprInitVal
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

evalDef (ArrDef stype (Ident arr) exprSize) = do
    let arrType = evalSimpleType stype
    (rhoV, rhoF) <- ask
    sto <- getStore
    (VInt size) <- evalExpr exprSize
    if size < 0 then throwError (InvalidArraySize size)
    else do
        let (loc, sto') = newloc sto
        let arrVal = replicate (fromInteger size) (if arrType == SimpleInt STInt then VInt 0 else VBool False)
        let rhoV' = mapSet rhoV arr (loc, (False, False))
        controlFlow <- getControlFlow
        put (setVarVal rhoV' sto' arr (ComplexVal (VArray arrVal)), controlFlow)
        return (rhoV', rhoF)

evalDef (DictDef stype (Ident dict)) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV dict (loc, (False, False))
    controlFlow <- getControlFlow
    put (setVarVal rhoV' sto' dict (ComplexVal (VDict empty)), controlFlow)
    return (rhoV', rhoF)

evalDef (FuncDef ftype (Ident func) params instr) = do
    (rhoV, rhoF) <- ask
    let x :: [FuncArg] -> WorkingMonad(SimpleValue) 
        x = \args -> do 
            let paramList = evalParams params
            (rhoV, rhoF) <- setArguments (paramList) args
            let rhoF' = mapSet rhoF func x
            (res, _, _) <- local (const (rhoV, rhoF')) (evalInstr instr)
            case res of
                Just val -> return (val)
                Nothing -> case (evalFuncReturnType ftype) of
                    SimpleInt STInt -> return (VInt 0)
                    SimpleBool STBool -> return (VBool False)
    let rhoF' = mapSet rhoF func x
    return (rhoV, rhoF')

setArguments :: [FuncParam] -> [FuncArg] -> WorkingMonad (VEnv, FMEnv)
setArguments [] [] = do
    (rhoV, rhoF) <- ask
    return (rhoV, rhoF)

setArguments (PSimple stype (Ident var) : restParams) (SimpleArg val : restArgs) = do
    (rhoV, rhoF) <- ask
    sto <- getStore
    let (loc, sto') = newloc sto
    let rhoV' = mapSet rhoV var (loc, (False, False))
    putStore (setVarVal rhoV' sto' var (SimpleVal val))
    local (const (rhoV', rhoF)) $ do
        setArguments restParams restArgs

setArguments (PFunc ftype (Ident func) : restParams) (FArg f : restArgs) = do
    (rhoV, rhoF) <- ask
    let rhoF' = mapSet rhoF func f
    local (const (rhoV, rhoF')) $ do
        setArguments restParams restArgs

---------------------------------- ARGUMENTS AND PARAMETERS ----------------------------------------

evalArguments :: Args -> WorkingMonad [FuncArg]


evalArguments (ArgsVoid) = return []
evalArguments (ArgsOne expr) = do
    val <- evalExpr expr
    return [SimpleArg val]
evalArguments (ArgsMany expr args) = do
    val <- evalExpr expr
    vals <- evalArguments args
    return (SimpleArg val : vals)
evalArguments (ArgsLambda lambda) = do
    f <- evalLambda lambda
    return [FArg f]
evalArguments (ArgsLambdaMany lambda args) = do
    f <- evalLambda lambda
    fs <- evalArguments args
    return (FArg f : fs)
evalArguments (ArgsFunc func) = do
    f <- getFunc func
    return [FArg f]
evalArguments (ArgsFuncMany func args) = do
    f <- getFunc func
    fs <- evalArguments args
    return (FArg f : fs)

evalLambda :: Lambda -> WorkingMonad MFunc

evalLambda (Lam ftype params instr) = do
    (rhoV, rhoF) <- ask
    let x :: [FuncArg] -> WorkingMonad(SimpleValue) 
        x = \args -> do 
            let paramList = evalParams params
            (rhoV, rhoF) <- setArguments (paramList) args
            (res, _, _) <- local (const (rhoV, rhoF)) (evalInstr instr)
            case res of
                Just val -> return (val)
                Nothing -> return (VInt 0)
    return x

-- Parameters
evalParams :: Params -> [FuncParam]

evalParams (ParamsNone) = []
evalParams (ParamVar stype (Ident var)) = [PSimple (evalSimpleType stype) (Ident var)]
evalParams (ParamVarMany stype (Ident var) params) = (PSimple (evalSimpleType stype) (Ident var)) : evalParams params
evalParams (ParamFunc ftype params (Ident func)) =
    let funcParams = evalParams params in
    let paramTypes = evalParamTypes funcParams in
    [PFunc (DetFunc paramTypes (evalFuncReturnType ftype)) (Ident func)]
evalParams (ParamFuncMany ftype params (Ident func) paramsMany) =
    let funcParams = evalParams params in
    let paramTypes = evalParamTypes funcParams in
    let rest = evalParams paramsMany in
    [PFunc (DetFunc paramTypes (evalFuncReturnType ftype)) (Ident func)] ++ rest

------------------------------------------ STATEMENTS ------------------------------------------

evalStmt :: Stmt -> WorkingMonad (Maybe SimpleValue)

evalStmt (SSkip) = return Nothing

evalStmt (SBreak exp0) = do
    (VInt n) <- evalExpr exp0
    (_, _, nestingLevel) <- getControlFlow
    if n < 0 then throwError (InvalidBreakArgument n)
    else do
        if nestingLevel < n then throwError (TooLargeBreakArgument n)
        else do
            putControlFlow (n, False, nestingLevel)
            return Nothing

evalStmt (SBreak1) = do
    (_, _, nestingLevel) <- getControlFlow
    if nestingLevel < 1 then throwError (TooLargeBreakArgument 1)
    else do
        putControlFlow (1, False, nestingLevel)
        return Nothing

evalStmt (SContinue exp0) = do
    (VInt n) <- evalExpr exp0
    (_, _, nestingLevel) <- getControlFlow
    if n < 0 then throwError (InvalidContinueArgument n)
    else do
        if nestingLevel < n then throwError (TooLargeContinueArgument n)
        else do
            putControlFlow (n, True, nestingLevel)
            return Nothing

evalStmt (SContinue0) = do
    (_, _, nestingLevel) <- getControlFlow
    if nestingLevel < 1 then throwError (TooLargeContinueArgument 1)
    else do
        putControlFlow (0, True, nestingLevel)
        return Nothing

evalStmt (SFuncCall (Ident func) args) = do
     f <- getFunc (Ident func)
     arguments <- evalArguments args
     _ <- f arguments
     return Nothing -- result is ignored in a simple function call

evalStmt (SIf expr i0 i1) = do
    (VBool b) <- evalExpr expr
    if b then do
        (res1, _, _) <- evalInstr i0
        return res1
    else do
        (res2, _, _) <- evalInstr i1
        return res2

evalStmt (SWhile expr i) = do
    (bc, cf, nl) <- getControlFlow
    putControlFlow (bc, cf, nl + 1)
    let x = do
        (VBool b) <- evalExpr expr
        if b then do
            (res, rhoV, rhoF) <- evalInstr i -- instructions in while loop can modify the environment
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

evalStmt (SFor (Ident var) exprFrom exprTo instr) = do
    (VInt from) <- evalExpr exprFrom
    (VInt to) <- evalExpr exprTo
    (rhoV, rhoF) <- evalDef (VarDef (STI STInt) (Ident var) (ENum from))
    (bc, cf, nl) <- getControlFlow
    putControlFlow (bc, cf, nl + 1)
    local (const (rhoV, rhoF)) $ do
        let x = do
            (VInt val) <- evalExpr (VarVal (Ident var))
            if val <= to then do
                (res, rhoV, rhoF) <- evalInstr instr
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

evalStmt (SReturn expr) = do
    val <- evalExpr expr
    return (Just val)

-- Print only the value without the type
evalStmt (SPrint expr) = do
    val <- evalExpr expr
    case val of
        VInt n -> liftIO $ print n
        VBool b -> liftIO $ print b
    return Nothing

evalStmt (VarAssign (Ident var) expr) = do
    val <- evalExpr expr
    msetVarVal var (SimpleVal val)
    return Nothing

evalStmt (VarAssignPlus (Ident var) expr) = do
    val <- evalExpr expr
    (VInt m) <- evalExpr (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (n + m)))
            return (Nothing)

evalStmt (VarAssignMinus (Ident var) expr) = do
    val <- evalExpr expr
    (VInt m) <- evalExpr (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (m - n)))
            return (Nothing)

evalStmt (VarAssignMul (Ident var) expr) = do
    val <- evalExpr expr
    (VInt m) <- evalExpr (VarVal (Ident var))
    case val of
        VInt n -> do
            msetVarVal var (SimpleVal (VInt (n * m)))
            return (Nothing)

evalStmt (VarAssignDiv (Ident var) expr) = do
    val <- evalExpr expr
    (VInt m) <- evalExpr (VarVal (Ident var))
    case val of
        VInt n -> do
            if n == 0 then throwError DivByZero
            else do
                msetVarVal var (SimpleVal (VInt (m `div` n)))
                return (Nothing)

evalStmt (VarAssignMod (Ident var) expr) = do
    val <- evalExpr expr
    (VInt m) <- evalExpr (VarVal (Ident var))
    case val of
        VInt n -> do
            if n == 0 then throwError ModByZero
            else do
                msetVarVal var (SimpleVal (VInt (m `mod` n)))
                return (Nothing)

evalStmt (VarInc (Ident var)) = evalStmt (VarAssignPlus (Ident var) (ENum 1))

evalStmt (VarDec (Ident var)) = evalStmt (VarAssignMinus (Ident var) (ENum 1))

evalStmt (ArrElSet (Ident arr) exprIndex exprVal) = do
    val <- evalExpr exprVal
    (VInt index) <- evalExpr exprIndex
    a <- getArray (Ident arr)
    if index < 0 || (fromInteger index) >= (length a) then throwError (IndexOutOfBounds index)
    else do
        msetVarVal arr (ComplexVal (VArray (replaceNth a (fromInteger index) val)))
        return (Nothing)

evalStmt (DictElSet (Ident dict) exprKey exprVal) = do
    val <- evalExpr exprVal
    (VInt key) <- evalExpr exprKey
    d <- getDict (Ident dict)
    msetVarVal dict (ComplexVal (VDict (mapSet d key val)))
    return (Nothing)


------------------------------------------ SPECIAL STATEMENTS ------------------------------------------

getVarLoc :: Ident -> WorkingMonad (Loc, DebugFlags)

getVarLoc (Ident var) = do
    (rhoV, _) <- ask
    case Data.Map.lookup var rhoV of
        Just (loc, flags) -> return (loc, flags)

evalSpecStmt :: SpecStmt -> WorkingMonad (VEnv, FMEnv)


evalSpecStmt (SSwap (Ident var1) (Ident var2)) = do
    (rhoV, rhoF) <- ask
    (loc1, _) <- getVarLoc (Ident var1)
    (loc2, _) <- getVarLoc (Ident var2)
    let rhoV' = setVarLoc (setVarLoc rhoV var1 loc2) var2 loc1
    return (rhoV', rhoF)

evalSpecStmt (DebugAssEnable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (readFlag, _)) <- getVarLoc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (readFlag, True))
    return (rhoV', rhoF)

evalSpecStmt (DebugAssDisable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (readFlag, _)) <- getVarLoc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (readFlag, False))
    return (rhoV', rhoF)

evalSpecStmt (DebugReadEnable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (_, writeFlag)) <- getVarLoc (Ident var)
    let rhoV' = mapSet rhoV var (loc, (True, writeFlag))
    return (rhoV', rhoF)

evalSpecStmt (DebugReadDisable (Ident var)) = do
    (rhoV, rhoF) <- ask
    (loc, (_, writeFlag)) <- getVarLoc (Ident var)
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

initialState :: (Store, (BreakCount, Bool, NestingLevel))
initialState = (sto0, (0, False, 0))

runProgram :: Instr -> IO (Either Error (Maybe SimpleValue))
runProgram instr = do
    res <- evalStateT (runReaderT (runExceptT (runWorkingMonad (evalInstr instr))) initialEnv) initialState
    case res of
        Left err -> return (Left err)
        Right (val, _, _) -> return (Right val)