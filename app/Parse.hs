module Parse (
  runLoxParser,
)
where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Types

type Parser = Parsec Void T.Text

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
      rhs <- optional (symbol "=" *> expr_)
      return $ case rhs of
        Nothing -> Located loc $ Ident lval
        Just e -> Located loc $ Assgn lval e

atom :: Parser Expr
atom = choice [parens expr_, literal, assgn]

expr_ :: Parser Expr
expr_ = makeExprParser atom operatorTable

commaSeparated :: Parser a -> Parser [a]
commaSeparated p = fromMaybe [] <$> optional parseP
 where
  parseP = (:) <$> p <*> many (symbol "," *> p)

args :: Parser [Expr]
args = commaSeparated expr

callExpr :: Parser Expr
callExpr = do
  e <- expr_
  go e
 where
  go e = do
    shouldCont <- isJust <$> optional (lookAhead (symbol "("))
    if not shouldCont
      then pure e
      else do
        void (symbol "(")
        newE <- Located (location e) . Call e <$> (args <* symbol ")")
        go newE

expr :: Parser Expr
expr = makeExprParser callExpr operatorTable

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
forStmt = do
  pre <- keyword "for" *> symbol "(" *> assignStmt
  cond <- expr
  post <- EvalExpr <$> (symbol ";" *> expr <* symbol ")")
  prog <- scope_
  pure (Scope [pre, While cond (post : prog)])

params :: Parser [String]
params = commaSeparated ident

funStmt :: Parser Decl
funStmt =
  Fun
    <$> (keyword "fun" *> ident <* symbol "(")
    <*> params
    <* symbol ")"
    <*> scope_

returnStmt :: Parser Decl
returnStmt = Return <$> (keyword "return" *> expr <* symbol ";")

decl :: Parser Decl
decl =
  choice
    [ assignStmt
    , scope
    , ifStmt
    , whileStmt
    , forStmt
    , funStmt
    , returnStmt
    , EvalExpr <$> (expr <* symbol ";")
    ]

program :: Parser [Decl]
program = some decl

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

runLoxParser :: String -> T.Text -> Either LoxError Program
runLoxParser fname input = mapLeft syntaxError $ runParser program fname input
