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
import Data.Maybe

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
runLine (Stmt s) env = (exec s env, Nothing)
runLine (Expr e) env = (env, Just (eval e env))

-- Executor
-- --------
exec :: Stmt -> Env -> Env
exec (SetStmt var e) env = H.insert var (eval e env) env
exec (SeqStmt [first, second]) env = exec second (exec first env)
exec (SeqStmt (first:rest)) env = exec (SeqStmt rest) (exec first env)
exec (FuncStmt name args expr) env = H.insert name (CloVal args expr env) env

-- Lifting Functions
-- -----------------

-- liftNumOp accepts a function over `Float`s (like +, -, *, /) and two Vals,
-- and returns the result of applying the function to the contents of each Val
-- (and putting the result into a new Val). If either of the input Vals are an
-- ExnVal, then this function returns ExnVal "Cannot lift.".

liftNumOp :: (Float -> Float -> Float) -> Val -> Val -> Val
liftNumOp f (ExnVal s) _ = ExnVal "Cannot lift."
liftNumOp f x (ExnVal s) = ExnVal "Cannot lift."
liftNumOp f (NumVal x) (NumVal y) = NumVal (f x y)
liftNumOp _ _ _  = undefined

liftNumFunc :: (Float -> Float) -> Val -> Val
liftNumFunc f (NumVal num) = NumVal (f num)
liftNumFunc f _ = ExnVal ("Cannot Lift")

-- Evaluator
-- ---------

eval :: Expr -> Env -> Val


-- ### NumExpr
eval (NumExpr e) env = NumVal e


-- ### ConstExpr
eval (ConstExpr e) env = case (H.lookup e consts) of 
	Nothing -> ExnVal ("Constant "++e++" is not defined.")
	Just a -> NumVal a


-- ### VarExpr
eval (VarExpr e) env = case (H.lookup e env) of 
	Nothing -> ExnVal ("Variable name "++e++ " is not defined.")
	Just a -> a


-- ### Operator Expressions (AddExpr, ...)
eval (AddExpr num1 num2) env = liftNumOp (+) (eval num1 env) (eval num2 env)
eval (SubtractExpr num1 num2) env = liftNumOp (-) (eval num1 env) (eval num2 env)
eval (MultiplyExpr num1 num2) env = liftNumOp (*) (eval num1 env) (eval num2 env)
eval (DivideExpr num1 num2) env = case eval num2 env of 
	(NumVal 0) -> ExnVal ("Division by zero.")
	(NumVal x) -> liftNumOp (/) (eval num1 env) (eval (NumExpr x) env)

-- ### Function Application Expressions
--Special Cases
eval (AppExpr "sin" [(ConstExpr "pi")]) env = liftNumFunc sin (NumVal pi)
eval (AppExpr "sin" [(NumExpr arg)]) env = liftNumFunc sin (NumVal arg)
eval (AppExpr "cos" [(ConstExpr "pi")]) env = liftNumFunc cos (NumVal pi)
eval (AppExpr "cos" [(NumExpr arg)]) env = liftNumFunc cos (NumVal arg)

--General Case
eval (AppExpr name exprs) env = case (H.lookup name env) of
	Nothing -> ExnVal ("Function name "++name++" is not defined.")
	Just (CloVal args ex env2) -> eval ex (funcEval args exprs env2)
	_ -> ExnVal ("Can only apply CloVals.")
	

--Helper function to add the arguments to the function environment
funcEval :: [String] -> [Expr] -> Env -> Env
funcEval (x:xs) (expr:exprs) env = funcEval xs exprs (exec (SetStmt x expr) env)
funcEval [x] [expr] env = exec (SetStmt x expr) env
funcEval _ _ env = env