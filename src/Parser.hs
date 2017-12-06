module Parser ( runParser
              , p_line
              ) where

-- Imports
-- =======

import Text.Megaparsec hiding (runParser)
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Text.Megaparsec.Char (string, oneOf, space)
import Text.Megaparsec.Lexer (signed, float, decimal)

import qualified Data.HashMap.Strict as H (keys)

-- Local
-- -----

import Common


-- Parser
-- ======

runParser :: Parser a -> String -> Either String a
runParser p s =
  case parse (p <* eof) "<stdin>" s of
    Right success -> Right success
    Left  err     -> Left $ parseErrorPretty err

p_line :: Parser Line
p_line =  Stmt <$> try p_stmt
      <|> Expr <$> p_expr

-- Statements
-- ----------

p_stmt :: Parser Stmt
p_stmt =  try (p_stmt' <* notFollowedBy (symbol ";"))
      <|> p_seqStmt

p_stmt' :: Parser Stmt
p_stmt' = p_setStmt

p_setStmt :: Parser Stmt
p_setStmt = SetStmt <$> p_var <*> (symbol ":=" *> p_expr)

p_seqStmt :: Parser Stmt
p_seqStmt = SeqStmt <$> sepBy1 p_stmt' (symbol ";")

-- Expressions
-- -----------

p_expr :: Parser Expr
p_expr = space *> makeExprParser term table <?> "expression"

table :: [[ Operator Parser Expr ]]
table = [ [ op "*" MultiplyExpr
          , op "/" DivideExpr
          ]
        , [ op "+" AddExpr
          , op "-" SubtractExpr
        ] ]

op :: String -> (Expr -> Expr -> Expr) -> Operator Parser Expr
op name f = InfixL (f <$ symbol name)

term :: Parser Expr
term =  symbol "(" *> p_expr <* symbol ")"
    <|> (space *> atom <* space)

atom :: Parser Expr
atom =  p_numExpr
    <|> p_constExpr
    <|> p_varExpr

p_varExpr :: Parser Expr
p_varExpr = VarExpr <$> p_var

p_var :: Parser String
p_var = some $ oneOf ['a'..'z']

p_constExpr :: Parser Expr
p_constExpr = choice . map p_const $ H.keys consts

p_const :: String -> Parser Expr
p_const s = ConstExpr <$> symbol s

p_numExpr :: Parser Expr
p_numExpr = NumExpr <$> p_number

p_number :: Parser Float
p_number =  try (signed (return ()) $ realToFrac <$> float)
        <|> fromIntegral <$> signed (return ()) decimal

symbol :: String -> Parser String
symbol s = space *> string s <* space

p_funcStmt :: Parser FuncStmt
p_funcStmt = undefined


