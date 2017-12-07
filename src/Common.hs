module Common where

import qualified Data.HashMap.Strict as H (HashMap, fromList)

-- Types
-- -----

data Line = Stmt Stmt | Expr Expr
  deriving Show

data Stmt = SetStmt  String Expr
			| SeqStmt  [Stmt]
			| FuncStmt String [String] Expr
  deriving (Eq, Show)

data Expr = NumExpr      Float
          | ConstExpr    String
          | VarExpr      String
          | AddExpr      Expr Expr
          | SubtractExpr Expr Expr
          | MultiplyExpr Expr Expr
          | DivideExpr   Expr Expr
	      | AppExpr 	 String [Expr]
	deriving (Eq, Show)

data Val = NumVal Float
		 | CloVal [String] Expr Env
		 | ExnVal String
 deriving Eq

-- -------------------
-- Typeclass instances
 
-- TODO: define instance for `Show Val` here
--
-- Don't forget to remove the Show from the deriving list in the data
-- declaration for Val!

instance Show Val where
	show (NumVal x) = show x
	show (ExnVal x) = show ("Error: "++x)
	show (CloVal params body env) = "<" ++ show params ++ ", "
					    ++ show body ++ ", "
					    ++ show env ++ ">"

type Env = H.HashMap String Val

-- Operators
-- ---------

consts :: H.HashMap String Float
consts = H.fromList [ ("pi", pi)
                    , ("e", exp 1)
                    ]
