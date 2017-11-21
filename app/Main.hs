module Main where

-- Imports
-- =======

import Data.Maybe (fromJust)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)
import qualified Data.HashMap.Strict as H (empty)

-- Local
-- -----

import Lib

-- Main
-- ====

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl H.empty
    ["-p"] -> parserTest
    ["-d"] -> replDebug H.empty
    _      -> putStrLn usage

-- REPL functions
-- --------------

repl :: Env -> IO ()
repl env = do
  input <- putStr "> " >> hFlush stdout >> getLine
  let result = run input env
  case result of
    Left  err     -> putStrLn err *> repl env
    Right success ->
      case success of
        (env', Nothing)  -> repl env'
        (env', Just val) -> (putStrLn $ show $ val) *> repl env'

-- ### Used for testing

replDebug :: Env -> IO ()
replDebug env = do
  input <- putStr "> " >> hFlush stdout >> getLine
  case runParser p_line input of
    Left  err -> putStrLn err *> replDebug env
    Right line -> do
      putStrLn "\nInput line parsed to:"
      putStrLn . show $ line
      let (env', mVal) = runLine line env
      case line of
        Stmt _ -> do
          putStrLn "\nEnvironment after running input statement: "
          putStrLn . show $ env'
        Expr _ -> do
          putStrLn "\nValue after evaluating input expression: "
          putStrLn . show . fromJust $ mVal
      putStr "\n"
      replDebug env'

parserTest :: IO ()
parserTest = do
  input <- putStr "> " >> hFlush stdout >> getLine
  case runParser p_line input of
    Left  err -> putStrLn err *> parserTest
    Right line -> do
      putStrLn "\nInput line parsed to:"
      putStrLn . show $ line
      putStr "\n"
      parserTest

-- Usage
-- -----

usage :: String
usage = "Usage: Run with no args for normal calculator mode.\n"
     ++ "       Run with the -p option to run the parser only.\n"
     ++ "       Run with the -d option to run the calculator in debug mode."
