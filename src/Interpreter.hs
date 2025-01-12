import Prelude

import System.IO (hFlush, stdout, stderr, hPutStrLn)
import System.Environment ( getArgs )
import System.Exit        ( exitFailure )

import TypeChecker (performTypeCheck)
import Executor (runProgram)

import FraJer.Par   ( pInstr, myLexer )


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