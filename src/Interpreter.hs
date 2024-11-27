module Main where

import Prelude

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import System.IO.Unsafe   ( unsafePerformIO )
import Control.Monad      ( when )

import Data.Map
import qualified GHC.Integer (leInteger) 

-- Syntactic categories given in the FraJer.cf file
import FraJer.Abs   ( SType(..), FType(..), Ident(..), Expr(..), Args(..), Params(..), Instr(..), Def(..), Stmt(..), Lambda(..) )
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

-- Dictionary mapping strings to simple values (not used in functions directly)
-- We only have integers as keys in our language, since we don't have strings
-- and booleans don't make much sense as keys.
type DictKey = Integer
type Dict = Map DictKey SimpleValue

-- Value can either be a simple value (Int/Bool) or a complex value (Array/Dict)
data Value = SimpleVal SimpleValue | ComplexVal ComplexValue deriving (Show)

-- Function arguments can be either simple values (Int, Bool) or functions themselves
data FuncArg = SimpleArg SimpleValue | FArg Func

-- Function takes a list of function arguments and a store, returns a new store and a simple value)
type Func = [FuncArg] -> Store -> (Store, SimpleValue)

------------------------------------------ ENVIRONMENTS -------------------------------------------

type Loc = Integer
type Var = String

type VEnv = Map Var Loc
type FEnv = Map Var Func
data Store = CStore {currMap :: Map Loc Value, nextLoc :: Loc} deriving Show

------------------------------------------ HELPER FUNCTIONS ---------------------------------------

newloc:: Store -> (Loc, Store)
newloc (CStore map loc) = (loc, CStore map (loc + 1))

getVarVal:: VEnv -> Store -> Var -> Value
getVarVal rhoV sto var =
  let loc = mapGet rhoV var in
    mapGet (currMap sto) loc

setVarVal:: VEnv -> Store -> Var -> Value -> Store
setVarVal rhoV sto var val =
  let loc = mapGet rhoV var in
  let map = mapSet (currMap sto) loc val in
    CStore map (nextLoc sto)

replaceNth :: [a] -> Int -> a -> [a]
replaceNth xs n newVal = Prelude.take n xs ++ [newVal] ++ Prelude.drop (n + 1) xs

------------------------------------------ SEMANTICS ----------------------------------------------


-----------------------------------------EXPRESSIONS ----------------------------------------------
-- Expressions can modify store too, because functions can be called.
eE :: Expr -> VEnv -> FEnv -> Store -> (Store, SimpleValue)

{-
FuncVal.   Expr2 ::= Ident "(" Args ")";
VarVal.    Expr2 ::= Ident;
-}

eE (ENum n) rhoV rhoF sto = (sto, VInt n)

eE (FuncVal (Ident func) args) rhoV rhoF sto =
    let f = mapGet rhoF func in
    let (sto', args') = eA args rhoV rhoF sto in
        f args' sto'

eE (VarVal (Ident var)) rhoV rhoF sto = (sto, x) where
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

eE (EPlus exp0 exp1) rhoV rhoF sto = (sto'', VInt (x + y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

eE (EMinus exp0 exp1) rhoV rhoF sto = (sto'', VInt (x - y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

-- TODO add error handling
eE (EDiv exp0 exp1) rhoV rhoF sto = (sto'', VInt (x `div` y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

eE (EMul exp0 exp1) rhoV rhoF sto = (sto'', VInt (x * y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

eE (EMod exp0 exp1) rhoV rhoF sto = (sto'', VInt (x `mod` y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

eE (EArray (Ident arr) exp0) rhoV rhoF sto =
        let ComplexVal (VArray a) = getVarVal rhoV sto arr in
        let (sto', VInt i) = eE exp0 rhoV rhoF sto in
            (sto', a !! (fromInteger i))

eE (EDict (Ident dict) exp0) rhoV rhoF sto =
        let ComplexVal (VDict d) = getVarVal rhoV sto dict in
        let (sto', VInt i) = eE exp0 rhoV rhoF sto in
            (sto', d ! i)


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
    (sto', VInt x)

eE (EPreInc (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x + 1))) in
        (sto', VInt (x + 1))

eE (EPostDec (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x - 1))) in
        (sto', VInt x)

eE (EPreDec (Ident var)) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let sto' = setVarVal rhoV sto var (SimpleVal (VInt (x - 1))) in
        (sto', VInt (x - 1))

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

eE (BTrue) rhoV rhoF sto = (sto, VBool True)
eE (BFalse) rhoV rhoF sto = (sto, VBool False)

-- Works for both integers and booleans
eE (EEq exp0 exp1) rhoV rhoF sto = (sto'', VBool (x == y)) where
    (sto', x) = eE exp0 rhoV rhoF sto
    (sto'', y) = eE exp1 rhoV rhoF sto'

eE (ENeq exp0 exp1) rhoV rhoF sto = (sto'', VBool (x /= y)) where
    (sto', x) = eE exp0 rhoV rhoF sto
    (sto'', y) = eE exp1 rhoV rhoF sto'

eE (ELt exp0 exp1) rhoV rhoF sto = (sto'', VBool (x < y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

-- We can't reuse the previous functions, because of the stupid function calls, which modify store.
eE (EGt exp0 exp1) rhoV rhoF sto = (sto'', VBool (x > y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

eE (ELeq exp0 exp1) rhoV rhoF sto = (sto'', VBool (x <= y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

eE (EGeq exp0 exp1) rhoV rhoF sto = (sto'', VBool (x >= y)) where
    (sto', VInt x) = eE exp0 rhoV rhoF sto
    (sto'', VInt y) = eE exp1 rhoV rhoF sto'

-- Now specific to booleans
eE (BNot exp0) rhoV rhoF sto = (sto', VBool (not x)) where
    (sto', VBool x) = eE exp0 rhoV rhoF sto

eE (BOr exp0 exp1) rhoV rhoF sto = (sto'', VBool (x || y)) where
    (sto', VBool x) = eE exp0 rhoV rhoF sto
    (sto'', VBool y) = eE exp1 rhoV rhoF sto'

eE (BAnd exp0 exp1) rhoV rhoF sto = (sto'', VBool (x && y)) where
    (sto', VBool x) = eE exp0 rhoV rhoF sto
    (sto'', VBool y) = eE exp1 rhoV rhoF sto'

eE (BXor exp0 exp1) rhoV rhoF sto = (sto'', VBool (x /= y)) where
    (sto', VBool x) = eE exp0 rhoV rhoF sto
    (sto'', VBool y) = eE exp1 rhoV rhoF sto'

eE (BDictHasKey (Ident dict) exp0) rhoV rhoF sto =
        let ComplexVal (VDict d) = getVarVal rhoV sto dict in
        let (sto', VInt i) = eE exp0 rhoV rhoF sto in
            (sto', VBool (mapHasKey d i))

------------------------------------------ INSTRUCTIONS -------------------------------------------
-- Instructions include definitions and statements, so they can modify everything.
-- StmtResult is either a store or a store and a value (returned from a return statement).
iI :: Instr -> VEnv -> FEnv -> Store -> (VEnv, FEnv, StmtResult)

{-
ISeq.      Instr ::= Instr1 ";" Instr; -- right associative
Def.       Instr1 ::= Def;
Stmt.      Instr1 ::= Stmt;
-}

-- if first instruction returns a value, we don't execute the second one.
iI (ISeq instr0 instr1) rhoV rhoF sto =
    let (rhoV', rhoF', res) = iI instr0 rhoV rhoF sto in
        case res of
            StoreOnly sto' -> iI instr1 rhoV' rhoF' sto'
            StoreAndValue sto' val -> (rhoV', rhoF', StoreAndValue sto' val)

iI (Def def) rhoV rhoF sto =
    let (rhoV', rhoF', sto') = iD def rhoV rhoF sto in
        (rhoV', rhoF', StoreOnly sto')

iI (Stmt stmt) rhoV rhoF sto = (rhoV, rhoF, iS stmt rhoV rhoF sto)


-------------------------------VARIABLE DEFINITIONS------------------------------------------------
-- Definitions also can modify everything.
iD :: Def -> VEnv -> FEnv -> Store -> (VEnv, FEnv, Store)

{-
VarDef.        Def1 ::= SType Ident "=" Expr;

ArrDefInit.    Def1 ::= "Array" SType Ident "[" Expr "]" "(" Expr ")"; -- initialized with last argument
ArrDef.        Def1 ::= "Array" SType Ident "[" Expr "]";
DictDef.       Def1 ::= "Dict" SType Ident;

-- functions can also have local variables inside, so we allow declarations in the function body
FuncDef.       Def1 ::= FType Ident "(" Params ")" "{" Instr "}";
-}

iD (VarDef stype (Ident var) expr) rhoV rhoF sto =
    let (sto', val) = eE expr rhoV rhoF sto in
    let (loc, sto'') = newloc sto' in
    let rhoV' = mapSet rhoV var loc in
        (rhoV', rhoF, setVarVal rhoV' sto'' var (SimpleVal val))

-- Arrays can be initialized with both boolean or integer values. Size must be an integer.
iD (ArrDefInit stype (Ident arr) exprSize exprInitVal) rhoV rhoF sto =
    let (sto', VInt size) = eE exprSize rhoV rhoF sto in
    let (sto'', val) = eE exprInitVal rhoV rhoF sto' in
    let (loc, sto''') = newloc sto'' in
    let arrVal = replicate (fromInteger size) val in
    let rhoV' = mapSet rhoV arr loc in
        (rhoV', rhoF, setVarVal rhoV' sto''' arr (ComplexVal (VArray arrVal)))

-- Now it's a bit tricky, because we need to know the type of the array to create it.
-- Integer arrays are initialized with zeros, boolean arrays with False.
iD (ArrDef stype (Ident arr) exprSize) rhoV rhoF sto =
    let (sto', VInt size) = eE exprSize rhoV rhoF sto in
    let (loc, sto'') = newloc sto' in
    let arrVal = replicate (fromInteger size) (if stype == STInt then VInt 0 else VBool False) in
    let rhoV' = mapSet rhoV arr loc in
        (rhoV', rhoF, setVarVal rhoV' sto'' arr (ComplexVal (VArray arrVal)))

-- Dictionaries are empty by default.
iD (DictDef stype (Ident dict)) rhoV rhoF sto =
    let (loc, sto') = newloc sto in
    let rhoV' = mapSet rhoV dict loc in
        (rhoV', rhoF, setVarVal rhoV' sto' dict (ComplexVal (VDict empty)))

-- TODO add return value. x args sto' must be a (store, value) pair.
iD (FuncDef ftype (Ident func) params instr) rhoV rhoF sto =
    (rhoV, mapSet rhoF func x, sto) where
        x args sto' =
            let paramList = eP params in
            let (rhoV', rhoF', sto'') = prepareEnvs paramList rhoV rhoF sto' in
            let (rhoV'', rhoF'', sto''') = assignArgs paramList args rhoV' rhoF' sto'' in
            let rhoF''' = mapSet rhoF func x in -- now recursion makes sense
            let (rhoV''', rhoF'''', StoreAndValue resSto resVal) = iI instr rhoV'' rhoF''' sto''' in
                (resSto, resVal)

----------------------------------ARGUMENTS AND PARAMETERS ----------------------------------------
eA :: Args -> VEnv -> FEnv -> Store -> (Store, [FuncArg])

{-
ArgsVoid. Args ::= "void";
ArgsOne.  Args ::= Expr;
ArgsMany. Args ::= Expr "," Args;
ArgsLambda. Args ::= Lambda;
ArgsLambdaMany. Args ::= Lambda "," Args;
-}

eA (ArgsVoid) rhoV rhoF sto = (sto, [])
eA (ArgsOne expr) rhoV rhoF sto = (sto', [SimpleArg val]) where
    (sto', val) = eE expr rhoV rhoF sto
eA (ArgsMany expr args) rhoV rhoF sto = (sto'', (SimpleArg val) : args') where
    (sto', val) = eE expr rhoV rhoF sto
    (sto'', args') = eA args rhoV rhoF sto'
eA (ArgsLambda lambda) rhoV rhoF sto = (sto, [FArg (eL lambda rhoV rhoF sto)])
eA (ArgsLambdaMany lambda args) rhoV rhoF sto = (sto', (FArg (eL lambda rhoV rhoF sto)) : args') where
    (sto', args') = eA args rhoV rhoF sto

-- Lam. Lambda ::= FType "lambda" "(" Params ")" "->" "{" Instr "}";
eL :: Lambda -> VEnv -> FEnv -> Store -> Func

eL (Lam ftype params instr) rhoV rhoF sto =
    x where
        x args sto' =
            let paramList = eP params in
            let (rhoV', rhoF', sto'') = prepareEnvs paramList rhoV rhoF sto' in
            let (rhoV'', rhoF'', sto''') = assignArgs paramList args rhoV' rhoF' sto'' in
            let (rhoF''', rhoV''', StoreAndValue resSto resVal) = iI instr rhoV'' rhoF'' sto''' in
                (resSto, resVal)

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
    let rhoV' = mapSet rhoV var loc in
        prepareEnvs params rhoV' rhoF sto'

-- Since lambda can be the only functional-argument in our language, we can create a dummy function
-- for now and assign it to the parameter.
prepareEnvs ((PFunc ftype (Ident func)):params) rhoV rhoF sto =
    let (loc, sto') = newloc sto in
    let rhoF' = mapSet rhoF func (\_ _ -> (sto', VInt 0)) in
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
assignArgs ((PFunc ftype (Ident func)):params) ((FArg f):args) rhoV rhoF sto =
    let rhoF' = mapSet rhoF func f in
        assignArgs params args rhoV rhoF' sto



------------------------------------------ STATEMENTS ---------------------------------------------
-- Statements do not modify the environment, but can modify the store. The return statement
-- can also return a value, so we take that into account in the function type.
data StmtResult = StoreOnly Store | StoreAndValue Store SimpleValue

iS :: Stmt -> VEnv -> FEnv -> Store -> StmtResult

{-
internal SIf. Stmt1 ::= "if" "(" Expr ")" "{" Stmt "}" "else" "{" Stmt "}";
sif1.       Stmt1 ::= "if" "(" Expr ")" "{" Stmt "}" "else" "{" Stmt "}";
sif2.       Stmt1 ::= "if" "(" Expr ")" "{" Stmt "}";
SWhile.     Stmt1 ::= "while" "(" Expr ")" "{" Stmt "}";
SFor.       Stmt1 ::= "for" "(" Ident "=" Expr "to" Expr ")" "{" Stmt "}";
-}

iS (SIf bex i1 i2) rhoV rhoF sto =
    let (sto', VBool b) = eE bex rhoV rhoF sto in
        if b then
            iS i1 rhoV rhoF sto'
        else
            iS i2 rhoV rhoF sto'

-- TODO add break and continue. In loops there can also be a return statement, so the actual
-- x takes Store and returns StmtResult.
iS (SWhile bex i) rhoV rhoF sto = x sto where
    x st = let (st', VBool b) = eE bex rhoV rhoF st in
        if b then
            let res = iS i rhoV rhoF st' in
                case res of
                    StoreOnly sto' -> x sto'
                    StoreAndValue sto' val -> StoreAndValue sto' val
        else
            StoreOnly st'

iS (SFor (Ident var) exprFrom exprTo stmt) rhoV rhoF sto =
    let
        (sto', VInt from) = eE exprFrom rhoV rhoF sto
        (sto'', VInt to) = eE exprTo rhoV rhoF sto'
        (loc, sto''') = newloc sto''
        rhoV' = mapSet rhoV var loc
        sto'''' = setVarVal rhoV' sto''' var (SimpleVal (VInt from))

        x st =
            let SimpleVal (VInt i) = getVarVal rhoV' st var in
            if i <= to then
                let res = iS stmt rhoV' rhoF st in
                    case res of
                        StoreOnly sto' ->
                            let st'' = setVarVal rhoV' sto' var (SimpleVal (VInt (i + 1)))
                            in x st''
                        StoreAndValue sto' val -> StoreAndValue sto' val
            else
                StoreOnly st
    in x sto''''


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

iS (SSkip) rhoV rhoF sto = StoreOnly sto

iS (SReturn expr) rhoV rhoF sto =
    let (sto', val) = eE expr rhoV rhoF sto in
        StoreAndValue sto' val

-- actually print the expression on the stdout
iS (SPrint expr) rhoV rhoF sto =
    let (sto', val) = eE expr rhoV rhoF sto
        _ = unsafePerformIO $ putStrLn $ show val
    in StoreOnly sto'

-- we don't modify the environment, so var -> loc mapping stays the same. Only the values change.
iS (SSwap (Ident var1) (Ident var2)) rhoV rhoF sto =
    let SimpleVal val1 = getVarVal rhoV sto var1 in
    let SimpleVal val2 = getVarVal rhoV sto var2 in
    let sto' = setVarVal rhoV sto var1 (SimpleVal val2) in
    let sto'' = setVarVal rhoV sto' var2 (SimpleVal val1) in
        StoreOnly sto''

-- TODO implement break and continue using additional flags.

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

iS (VarAssign (Ident var) expr) rhoV rhoF sto =
    let (sto', val) = eE expr rhoV rhoF sto in
    let sto'' = setVarVal rhoV sto' var (SimpleVal val) in
        StoreOnly sto''

iS (VarAssignPlus (Ident var) expr) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let (sto', VInt y) = eE expr rhoV rhoF sto in
    let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x + y))) in
        StoreOnly sto''

iS (VarAssignMinus (Ident var) expr) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let (sto', VInt y) = eE expr rhoV rhoF sto in
    let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x - y))) in
        StoreOnly sto''

iS (VarAssignMul (Ident var) expr) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let (sto', VInt y) = eE expr rhoV rhoF sto in
    let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x * y))) in
        StoreOnly sto''

-- TODO add error handling
iS (VarAssignDiv (Ident var) expr) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let (sto', VInt y) = eE expr rhoV rhoF sto in
    let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x `div` y))) in
        StoreOnly sto''

iS (VarAssignMod (Ident var) expr) rhoV rhoF sto =
    let SimpleVal (VInt x) = getVarVal rhoV sto var in
    let (sto', VInt y) = eE expr rhoV rhoF sto in
    let sto'' = setVarVal rhoV sto' var (SimpleVal (VInt (x `mod` y))) in
        StoreOnly sto''

iS (ArrElSet (Ident arr) exprIndex exprVal) rhoV rhoF sto =
    let ComplexVal (VArray a) = getVarVal rhoV sto arr in
    let (sto', VInt i) = eE exprIndex rhoV rhoF sto in
    let (sto'', val) = eE exprVal rhoV rhoF sto' in
    let a' = replaceNth a (fromInteger i) val in
    let sto''' = setVarVal rhoV sto'' arr (ComplexVal (VArray a')) in
        StoreOnly sto'''

-- If the key is not in the dictionary, we add it. Otherwise we update the value.
iS (DictElSet (Ident dict) exprIndex exprVal) rhoV rhoF sto =
    let ComplexVal (VDict d) = getVarVal rhoV sto dict in
    let (sto', VInt i) = eE exprIndex rhoV rhoF sto in
    let (sto'', val) = eE exprVal rhoV rhoF sto' in
    let d' = insert i val d in
    let sto''' = setVarVal rhoV sto'' dict (ComplexVal (VDict d')) in
        StoreOnly sto'''










-- Example usage of the interpreter
main :: IO ()
main = do
    getContents >>= compute
    putStrLn ""

rhoF0:: FEnv
rhoF0 = fromList []

rhoV0:: VEnv
rhoV0 = fromList []

sto0:: Store
sto0 = CStore empty 0

compute s =
    case pInstr (myLexer s) of
        Left err -> do
            putStrLn "\nParse              Failed...\n"
            putStrLn err
            exitFailure
        Right e -> do
            putStrLn "\nParse Successful!"
            let (rhoF', rhoV', sto') = iI e rhoV0 rhoF0 sto0 in
                case sto' of
                    StoreOnly sto'' -> putStrLn $ show sto''
                    -- Show store and the value returned from the return statement
                    StoreAndValue sto'' val -> putStrLn $ show sto'' ++ " " ++ show val