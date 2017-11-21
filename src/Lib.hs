module Lib ( run
           , exec
           , eval
           , runLine
           , runParser
           , p_line
           , module Common
           ) where

-- Imports
-- =======

import qualified Data.HashMap.Strict as H (insert, lookup)

-- Local
-- ----

import Common
import Parser (runParser, p_line)

-- Functionality
-- =============

-- run takes in a user's input string and tries to parse into a line, then run
-- the line it in the given environment.

run :: String -> Env -> Either String (Env, Maybe Val)
run input env = runInput (runParser p_line input) env

-- runInput is the interface between runParser and the executor + evaluator

runInput :: Either String Line -> Env -> Either String (Env, Maybe Val)
runInput (Right line) env = Right $ runLine line env
runInput (Left err)   _   = Left err

-- runLine accepts a Line, which is either a Stmt or an Expr, then returns the
-- result of executing (for Stmt) or evaluating (for Expr) that line.

runLine :: Line -> Env -> (Env, Maybe Val)
runLine = undefined

-- Executor
-- --------

exec :: Stmt -> Env -> Env
exec _ _ = undefined

-- Lifting Functions
-- -----------------

-- liftNumOp accepts a function over `Float`s (like +, -, *, /) and two Vals,
-- and returns the result of applying the function to the contents of each Val
-- (and putting the result into a new Val). If either of the input Vals are an
-- ExnVal, then this function returns ExnVal "Cannot lift.".

liftNumOp :: (Float -> Float -> Float) -> Val -> Val -> Val
liftNumOp _ _ _ = undefined

-- Evaluator
-- ---------

eval :: Expr -> Env -> Val
eval _ _ = undefined

-- ### NumExpr

-- ### ConstExpr

-- ### VarExpr

-- ### Operator Expressions (AddExpr, ...)

-- remember to use liftNumOp here
