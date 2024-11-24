module Main where

import Prelude

import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when )

import Data.Map
import qualified GHC.Integer (leInteger) 

-- Syntactic categories given in the FraJer.cf file
import FraJer.Abs   ( Type(..), FType(..), Ident(..), Expr(..), Args(..), Params(..), Instr(..), Def(..), Stmt(..) )
import FraJer.Lex   ( Token, mkPosToken )
import FraJer.Par   ( pExpr, pInstr, pDef, pStmt, myLexer )
import FraJer.Print ( Print, printTree )
import FraJer.Skel  ()

mapGet :: (Ord k) => (Map k v) -> k -> v
mapGet map arg = map ! arg

mapSet :: (Ord k) => (Map k v) -> k -> v -> (Map k v)
mapSet map arg val = insert arg val map

mapHasKey :: (Ord k) => (Map k v) -> k -> Bool
mapHasKey map arg = member arg map

------------------------------------------ TYPES (sic!) -------------------------------------------

data Type = TSimple SimpleType | TComplex ComplexType | TFunc FuncType deriving (Show, Eq)

-- Simple types(int, bool)
data SimpleType = TInt | TBool deriving (Show, Eq)

-- Complex types (arrays and dictionaries)
data ComplexType = TArray SimpleType | TDict SimpleType deriving (Show, Eq)

data FuncParam = SType SimpleType | PFunc FuncType deriving (Show, Eq)

-- Function types (can take functions as arguments, but return only simple types)
data FuncType = TFunc [FuncParam] SimpleType deriving (Show, Eq)

------------------------------------------ DATATYPES ----------------------------------------------

-- Simple values (Int, Bool)
data SimpleValue = VInt Integer | VBool Bool deriving (Show, Eq)

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
data FuncArg = SimpleArg SimpleValue | FArg Func deriving (Show)

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

------------------------------------------ SEMANTICS ----------------------------------------------

----------------------------------ARGUMENTS AND PARAMETERS ----------------------------------------
eA :: Args -> VEnv -> FEnv -> Store -> [FuncArg]

{-
ArgsVoid. Args ::= "void";
ArgsOne.  Args ::= Expr;
ArgsMany. Args ::= Expr "," Args;
ArgsLambda. Args ::= Lambda;
ArgsLambdaMany. Args ::= Lambda "," Args;
-}

eA (ArgsVoid) rhoV rhoF sto = []
eA (ArgsOne expr) rhoV rhoF sto = [SimpleArg (eE expr rhoV rhoF sto)]
eA (ArgsMany expr args) rhoV rhoF sto = (SimpleArg (eE expr rhoV rhoF sto)) : (eA args rhoV rhoF sto)
-- TODO lambda evaluation
eA (ArgsLambda lambda) rhoV rhoF sto = [FuncArg (eL lambda rhoV rhoF sto)]
eA (ArgsLambdaMany lambda args) rhoV rhoF sto = (FuncArg (eL lambda rhoV rhoF sto)) : (eA args rhoV rhoF sto)

-- and parameters
eP :: Params -> VEnv -> FEnv -> [FuncParam]

{-
ParamsNone.      Params ::= "none";
ParamVar.        Params ::= Type VarIdent;
ParamFunc.       Params ::= FType FuncIdent;
ParamVarMany.    Params ::= Type VarIdent "," Params;
ParamFuncMany.   Params ::= FType FuncIdent "," Params;
-}

eP (ParamsNone) rhoV rhoF = []
eP (ParamVar (TSimple st) (Ident var)) rhoV rhoF = [SType st]
eP (ParamFunc (TFunc fparams st) (Ident func)) rhoV rhoF = [PFunc (TFunc fparams st)]
eP (ParamVarMany (TSimple st) (Ident var) params) rhoV rhoF = (SType st) : (eP params rhoV rhoF)
eP (ParamFuncMany (TFunc fparams st) (Ident func) params) rhoV rhoF = (PFunc (TFunc fparams st)) : (eP params rhoV rhoF)

-----------------------------------------EXPRESSIONS ----------------------------------------------
-- Expressions can modify store too, because functions can be called.
eE :: Expr -> VEnv -> FEnv -> Store -> (Store, SimpleValue)

{-
FuncVal.   Expr2 ::= FuncIdent "(" Args ")";
VarVal.    Expr2 ::= VarIdent;
-}

eE (ENum n) rhoV rhoF sto = (sto, VInt n)

eE (FuncVal (Ident func) args) rhoV rhoF sto = (sto', x) where
    f = mapGet rhoF func
    (sto', x) = f (eA args rhoV rhoF sto) sto

eE (VarVal (Ident var)) rhoV rhoF sto = (sto, x) where
  SimpleVal x = getVarVal rhoV sto var

{-
EPlus.   Expr  ::= Expr "+" Expr1;
EMinus.  Expr  ::= Expr "-" Expr1;
EDiv.    Expr1 ::= Expr1 "/" Expr2;
EMul.    Expr1 ::= Expr1 "*" Expr2;
EMod.    Expr1 ::= Expr1 "%" Expr2;
ENum.    Expr2 ::= Integer;

EArray.  Expr2 ::= ArrIdent "[" Expr "]";
EDict.   Expr2 ::= DictIdent "get" "[" Expr "]";
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
        let VArray a = getVarVal rhoV sto arr in
        let (sto', VInt i) = eE exp0 rhoV rhoF sto in
            (sto', a !! fromInteger i)

eE (EDict (Ident dict) exp0) rhoV rhoF sto =
        let VDict d = getVarVal rhoV sto dict in
        let (sto', VInt i) = eE exp0 rhoV rhoF sto in
            (sto', mapGet d i)


-- Expressions with side effects.
{-
EPostInc. Expr2 ::= VarIdent "++";
EPreInc.  Expr2 ::= "++" VarIdent;
EPostDec. Expr2 ::= VarIdent "--";
EPreDec.  Expr2 ::= "--" VarIdent;
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

BDictHasKey.  Expr2 ::= DictIdent "has" "key" "[" Expr "]";
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
        let VDict d = getVarVal rhoV sto dict in
        let (sto', VInt i) = eE exp0 rhoV rhoF sto in
            (sto', VBool (mapHasKey d i))

------------------------------------------ INSTRUCTIONS -------------------------------------------
-- Instructions include definitions and statements, so they can modify everything.
iI :: Instr -> VEnv -> FEnv -> Store -> (VEnv, FEnv, Store)

{-
ISeq.      Instr ::= Instr1 ";" Instr; -- right associative
Def.       Instr1 ::= Def;
Stmt.      Instr1 ::= Stmt;
-}

iI (ISeq instr0 instr1) rhoV rhoF sto =
  let (rhoV', rhoF', sto') = iI instr0 rhoV rhoF sto in
    iI instr1 rhoV' rhoF' sto'

-------------------------------VARIABLE AND FUNCTION DEFINITIONS-----------------------------------
-- Definitions also can modify everything.
iD :: Def -> FEnv -> VEnv -> Store -> (FEnv, VEnv, Store)

{-
VarDef.        Def1 ::= Type VarIdent "=" Expr;

ArrDefInit.    Def1 ::= "Array" Type ArrIdent "[" Expr "]" "(" Expr ")"; -- initialized with last argument
ArrDef.        Def1 ::= "Array" Type ArrIdent "[" Expr "]";
DictDef.       Def1 ::= "Dict" Type DictIdent;

-- functions can also have local variables inside, so we allow declarations in the function body
FuncDef.       Def1 ::= FType FuncIdent "(" Params ")" "{" Instr "}";
-}

iD (VarDef (TSimple stype) (Ident var) expr) rhoF rhoV sto =
    let (val, sto') = eE expr rhoV rhoF sto in
    let (loc, sto'') = newloc sto' in
    let rhoV' = mapSet rhoV var loc in
        (rhoF, rhoV', setVarVal rhoV' sto'' var (SimpleVal val))

-- Arrays can be initialized with both boolean or integer values. Size must be an integer.
iD (ArrDefInit (TSimple stype) (Ident arr) exprSize exprInitVal) rhoF rhoV sto =
    let (sto', VInt size) = eE exprSize rhoV rhoF sto in
    let (sto'', val) = eE exprInitVal rhoV rhoF sto' in
    let (loc, sto''') = newloc sto'' in
    let arr = replicate (fromInteger size) val in
    let rhoV' = mapSet rhoV arr loc in
        (rhoF, rhoV', setVarVal rhoV' sto''' arr (ComplexVal (VArray arr)))

-- Now it's a bit tricky, because we need to know the type of the array to create it.
-- Integer arrays are initialized with zeros, boolean arrays with False.
iD (ArrDef (TSimple stype) (Ident arr) exprSize) rhoF rhoV sto =
    let (sto', VInt size) = eE exprSize rhoV rhoF sto in
    let (loc, sto'') = newloc sto' in
    let arr = replicate (fromInteger size) (if stype == TInt then VInt 0 else VBool False) in
    let rhoV' = mapSet rhoV arr loc in
        (rhoF, rhoV', setVarVal rhoV' sto'' arr (ComplexVal (VArray arr)))

-- Dictionaries are empty by default.
iD (DictDef (TSimple stype) (Ident dict)) rhoF rhoV sto =
    let (loc, sto') = newloc sto in
    let rhoV' = mapSet rhoV dict loc in
        (rhoF, rhoV', setVarVal rhoV' sto' dict (ComplexVal (VDict Map.empty)))




















-- Semantics of statements
iS :: Stmt -> FEnv -> VEnv -> Store -> Store

iS (SAssgn (Ident var) expr) rhoF rhoV sto =
  let loc = mapGet rhoV var in
  let val = eE expr rhoV sto in
    setVar rhoV sto var val

iS (SSkip) rhoF rhoV sto = sto

iS (SIf bex i1 i2) rhoF rhoV sto = if eB bex rhoV sto
                          then iS i1 rhoF rhoV sto
                          else iS i2 rhoF rhoV sto
                          
  -- x :: Store -> Store
iS (SWhile bex i) rhoF rhoV sto = x sto where
    x st = if eB bex rhoV st then x (iS i rhoF rhoV st) else st
    
iS (SSeq i1 i2) rhoF rhoV sto = iS i2 rhoF rhoV (iS i1 rhoF rhoV sto)

{-
SCall.      Stmt2 ::= "call" Ident "(" Expr ")";
SBlock.     Stmt2 ::= "begin" Decl "in" Stmt "end";
-}

iS (SCall (Ident proc) expr) rhoF rhoV sto =
  let p = mapGet rhoF proc in
    p (eE expr rhoV sto) sto

iS (SBlock decl stmt) rhoF rhoV sto =
  let (rhoF', rhoV', sto') = iD decl rhoF rhoV sto in
    iS stmt rhoF' rhoV' sto'

-- Semantics of declarations
{-
DVar.	Decl1 ::= "var" Ident "=" Expr;
DFunc.	Decl1 ::= "proc" Ident "(" Ident ")" "{" Stmt "}";
DSeq.	Decl ::= Decl1 ";" Decl;
-}
iD :: Decl -> FEnv -> VEnv -> Store -> (FEnv, VEnv, Store)

iD (DVar (Ident var) expr) rhoF rhoV sto =
  let (loc, sto') = newloc sto in
  let val = eE expr rhoV sto in
  let rhoV' = mapSet rhoV var loc in
    (rhoF, rhoV', setVar rhoV' sto' var val)

-- Call by value with recursive calls
iD (DFunc (Ident proc) (Ident var) stmt) rhoF rhoV sto =
  (mapSet rhoF proc x, rhoV, sto) where
    x n s' = let (l, s'') = newloc s' in
      let rhoV' = mapSet rhoV var l in
      let s''' = setVar rhoV' s'' var n in
       iS stmt (mapSet rhoF proc x) rhoV' s'''

iD (DSeq decl0 decl1) rhoF rhoV sto =
  let (rhoF', rhoV', sto') = iD decl0 rhoF rhoV sto in
    iD decl1 rhoF' rhoV' sto'

main :: IO ()
main = do
    getContents >>= compute
    putStrLn ""

rhoF0:: FEnv
rhoF0 = fromList []

rhoV0:: VEnv
rhoV0 = fromList [("x", 0), ("y", 1), ("z", 2)]

sto0:: Store
sto0 = CStore (fromList [(0, 3), (1, 2), (2, 3)]) 3

compute s =
    case pStmt (myLexer s) of
        Left err -> do
            putStrLn "\nParse              Failed...\n"
            putStrLn err
            exitFailure
        Right e -> do
            putStrLn "\nParse Successful!"
            putStrLn $ show (iS e rhoF0 rhoV0 sto0)
