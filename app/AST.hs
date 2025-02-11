module AST (
  BinOp (..),
  UnOp (..),
  Value (..),
  Expr_ (..),
  Expr,
  SourcePos (..),
  Located (..),
  Stmt (..),
  Decl (..),
  Program,
  decl,
  expr,
  stmt,
  program,
  showValuePretty,
)
where

import Control.Applicative ((<**>))
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)

data BinOp = Dot | Add | Sub | Mul | Div | Or | And | Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show)
data UnOp = Negate | Not
  deriving (Show)
data Value = TNum Double | TString String | TBool Bool | TNil
  deriving (Show)
data Expr_
  = Ident String
  | Value Value
  | Assgn String Expr
  | UnOp UnOp Expr
  | BinOp BinOp Expr Expr
  deriving (Show)

data Located a = Located
  { location :: SourcePos
  , unLocate :: a
  }
  deriving (Functor)

instance (Show a) => Show (Located a) where
  show (Located l a) = "(" ++ sourcePosPretty l ++ " " ++ show a ++ ")"

showValuePretty :: Value -> String
showValuePretty (TNum x) = show x
showValuePretty (TString s) = show s
showValuePretty (TBool b) = show b
showValuePretty TNil = "nil"

data Stmt = Print Expr | EvalExpr Expr
  deriving (Show)

data Decl
  = Bind String Expr
  | Scope [Decl]
  | Stmt Stmt
  | If Expr [Decl] (Maybe [Decl])
  | While Expr [Decl]
  | For Expr Expr Expr [Decl]
  deriving (Show)

type Expr = Located Expr_
type Parser = Parsec Void T.Text
type Program = [Decl]

withLocation :: Parser Expr_ -> Parser Expr
withLocation p = Located <$> getSourcePos <*> p

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

keyword :: T.Text -> Parser T.Text
keyword word = lexeme . try $ string word <* notFollowedBy alphaNumChar

ident :: Parser String
ident = lexeme ((:) <$> letterChar <*> many alphaNumChar)

literal :: Parser Expr
literal =
  withLocation $
    choice
      [ Value . TNum <$> lexeme (try L.float <|> L.decimal)
      , Value . TString <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))
      , Value (TBool True) <$ keyword "true"
      , Value (TBool False) <$ keyword "false"
      , Value TNil <$ keyword "nil"
      ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

assgn :: Parser Expr
assgn = do
  loc <- getSourcePos
  lval <- ident
  equality <- isJust <$> optional (lookAhead (symbol "=="))
  if equality
    then pure $ Located loc $ Ident lval
    else do
      rhs <- optional (symbol "=" *> expr)
      return $ case rhs of
        Nothing -> Located loc $ Ident lval
        Just e -> Located loc $ Assgn lval e

atom :: Parser Expr
atom = choice [parens expr, literal, assgn]

expr :: Parser Expr
expr = makeExprParser atom operatorTable

binary :: T.Text -> (Expr -> Expr -> Expr_) -> Operator Parser Expr
binary name f = InfixL $ do
  l <- getSourcePos <* keyword name
  return $ \e1 e2 -> Located l (f e1 e2)

prefix :: T.Text -> (Expr -> Expr_) -> Operator Parser Expr
prefix name f = Prefix $ do
  l <- getSourcePos <* keyword name
  return $ \e -> Located l (f e)

-- decreasing precedence
-- there is a tradeoff here with the library where multiple occurences of
-- prefix operators don't work, e.g., !!false. it's pretty clear in the source
-- (https://hackage.haskell.org/package/parser-combinators-1.3.0/docs/src/Control.Monad.Combinators.Expr_.html#pTerm)
-- where this arises, but i cbs rolling my own
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [binary "." (BinOp Dot)]
  ,
    [ prefix "-" (UnOp Negate)
    , prefix "!" (UnOp Not)
    ]
  ,
    [ binary "*" (BinOp Mul)
    , binary "/" (BinOp Div)
    ]
  ,
    [ binary "+" (BinOp Add)
    , binary "-" (BinOp Sub)
    ]
  ,
    [ binary "<=" (BinOp Leq)
    , binary "<" (BinOp Lt)
    , binary ">=" (BinOp Geq)
    , binary ">" (BinOp Gt)
    ]
  ,
    [ binary "==" (BinOp Eq)
    , binary "!=" (BinOp Neq)
    ]
  ,
    [ binary "and" (BinOp And)
    , binary "or" (BinOp Or)
    ]
  ]

terminal :: Parser ()
terminal = void (symbol ";")

printStmt :: Parser Stmt
printStmt = Print <$> (keyword "print" *> expr <* terminal)

evalStmt :: Parser Stmt
evalStmt = EvalExpr <$> (expr <* terminal)

stmt :: Parser Stmt
stmt = printStmt <|> evalStmt

assignStmt :: Parser Decl
assignStmt = do
  void $ keyword "var"
  name <- ident
  l <- getSourcePos
  e <- optional (symbol "=" *> expr)
  void terminal
  return $ Bind name (fromMaybe (Located l (Value TNil)) e)

scope_ :: Parser [Decl]
scope_ = symbol "{" *> program <* symbol "}"

scope :: Parser Decl
scope = Scope <$> scope_

condition :: Parser Expr
condition = symbol "(" *> expr <* symbol ")"

ifStmt :: Parser Decl
ifStmt =
  If
    <$> (keyword "if" *> condition)
    <*> scope_
    <*> optional (keyword "else" *> scope_)

whileStmt :: Parser Decl
whileStmt =
  While
    <$> (keyword "while" *> condition)
    <*> scope_

forStmt :: Parser Decl
forStmt =
  For
    <$> (keyword "for" *> symbol "(" *> expr)
    <*> (symbol ";" *> expr)
    <*> (symbol ";" *> expr <* symbol ")")
    <*> scope_

decl :: Parser Decl
decl = assignStmt <|> scope <|> ifStmt <|> whileStmt <|> forStmt <|> (Stmt <$> stmt)

program :: Parser [Decl]
program = some decl
