-- TODOS:
-- > Tests to reconsider:
-- > good ()
-- > bad (103)


import Prelude

import System.IO (readFile, hFlush, stdout, stderr, hPutStrLn)
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )
import Control.Monad      ( when, ap, liftM )
import Control.Monad.Reader ( Reader, ReaderT, MonadReader, MonadIO, runReader, runReaderT, ask, local, liftIO, ap, liftM, lift )
import Control.Monad.State  ( State, StateT, MonadState, MonadIO, evalState, evalStateT, get, put, liftIO, ap, liftM, lift )
import Control.Monad.Except ( ExceptT, MonadError, MonadIO, runExceptT, throwError, catchError, liftIO, ap, liftM, lift )
import Control.Monad.Identity ( Identity, runIdentity, ap, liftM )

import TypeChecker (performTypeCheck)
import Executor (runProgram)

import Data.Map
import qualified GHC.Integer (leInteger) 

-- Syntactic categories given in the FraJer.cf file
import FraJer.Abs   ( SSTInt(..), SSTBool(..), FFTInt(..), FFTBool(..), SimpleType(..), FuncType(..),
                      Ident(..), Expr(..), Args(..), Params(..),
                      Instr(..), Def(..), Stmt(..), SpecStmt(..), Lambda(..) )
import FraJer.Lex   ( Token, mkPosToken )
import FraJer.Par   ( pExpr, pInstr, pDef, pStmt, pLambda, myLexer )
import FraJer.Print ( Print, printTree )


-- Example usage of the interpreter
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> runFile filePath
        _ -> putStrLn "Usage: interpreter <file>"



runFile :: FilePath -> IO ()
runFile path = do
    content <- readFile path
    case pInstr (myLexer content) of
        Left err -> do
            putStrLn "Parse Failed...\n"
            hPutStrLn stderr err
            exitFailure
        Right e -> do
            putStrLn "Parse Successful!\n"

            typeCheckResult <- performTypeCheck e
            
            case typeCheckResult of
                Left err -> do
                    putStrLn "Type Check Failed..." >> hFlush stdout
                    hPutStrLn stderr $ "Error: " ++ show err
                    exitFailure
                Right _ -> do
                    putStrLn "Type Check Successful!\n" >> hFlush stdout

                    result <- runProgram e

                    case result of
                        Left err -> do
                            putStrLn "Computation Failed..." >> hFlush stdout
                            hPutStrLn stderr $ "Error: " ++ show err
                            exitFailure
                        Right res -> do
                            case res of
                                Just val -> do
                                    putStrLn "Computation Successful!" >> hFlush stdout
                                    putStrLn $ "Program returned with value: " ++ show val
                                Nothing -> do
                                    putStrLn "Computation Successful!\n" >> hFlush stdout
                                    putStrLn "Program returned without a value."