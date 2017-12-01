module Common where

import qualified Data.HashMap.Strict as H (HashMap, fromList)

-- Types
-- -----

data Line = Stmt Stmt | Expr Expr
  deriving Show

data Stmt = SetStmt  String Expr
          | SeqStmt  [Stmt]
  deriving Show

data Expr = NumExpr      Float
          | ConstExpr    String
          | VarExpr      String
          | AddExpr      Expr Expr
          | SubtractExpr Expr Expr
          | MultiplyExpr Expr Expr
          | DivideExpr   Expr Expr
  deriving Show

data Val = NumVal Float
         | ExnVal String
 deriving (Eq, Show)

-- Typeclass instances
-- -------------------
 
-- TODO: define instance for `Show Val` here
--
-- Don't forget to remove the Show from the deriving list in the data
-- declaration for Val!



type Env = H.HashMap String Val

-- Operators
-- ---------

consts :: H.HashMap String Float
consts = H.fromList [ ("pi", pi)
                    , ("e", exp 1)
                    ]
