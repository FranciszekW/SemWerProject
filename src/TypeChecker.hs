{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TypeChecker
    ( SType(..), Type(..), ComplexType(..), FuncParam(..), DetailedFuncType(..),
      TVEnv, TFEnv, TypeStore(..), TypeMonad(..),
      typeof, evalSimpleType, evalFuncReturnType, evalParamTypes,
      checkInstr
    ) where

import Prelude

import System.IO (readFile, hFlush, stdout, stderr, hPutStrLn)
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when, ap, liftM )
import Control.Monad.Reader ( Reader, ReaderT, MonadReader, MonadIO, runReader, runReaderT, ask, local, liftIO, ap, liftM, lift )
import Control.Monad.State  ( State, StateT, MonadState, MonadIO, evalState, evalStateT, get, put, liftIO, ap, liftM, lift )
import Control.Monad.Except ( ExceptT, MonadError, MonadIO, runExceptT, throwError, catchError, liftIO, ap, liftM, lift )
import Control.Monad.Identity ( Identity, runIdentity, ap, liftM )

import Data.Map
import qualified GHC.Integer (leInteger)

import FraJer.Abs   ( SSTInt(..), SSTBool(..), FFTInt(..), FFTBool(..), SimpleType(..), FuncType(..),
                      Ident(..), Expr(..), Args(..), Params(..),
                      Instr(..), Def(..), Stmt(..), SpecStmt(..), Lambda(..) )

type Var = String
type Loc = Integer
type DictKey = Integer

data SimpleValue = VInt Integer | VBool Bool deriving (Show)

instance Eq SimpleValue where
  VInt x == VInt y = x == y
  VBool x == VBool y = x == y
  _ == _ = False

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