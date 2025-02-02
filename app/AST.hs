module AST (BinOp (..), UnOp (..), Value (..), Expr_ (..), Expr, expr, SourcePos (..))
where

import Control.Applicative ((<**>))
import Control.Monad.Combinators.Expr
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data BinOp = Dot | Add | Sub | Mul | Div | Or | And | Eq | Neq | Lt | Leq | Gt | Geq
  deriving (Show)
data UnOp = Negate | Not
  deriving (Show)
data Value = TNum Double | TString String | TBool Bool | TNil
  deriving (Show)
data Expr_ a
  = Ident String a
  | Value Value a
  | UnOp UnOp Expr a
  | BinOp BinOp Expr Expr a
  deriving (Show)

type Expr = Expr_ SourcePos

type Parser = Parsec Void T.Text

withLocation :: Parser (SourcePos -> Expr) -> Parser Expr
withLocation p = getSourcePos <**> p

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

var :: Parser Expr
var = withLocation $ Ident <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

literal :: Parser Expr
literal =
  withLocation $
    choice
      [ Value . TNum <$> lexeme (try L.float <|> L.decimal)
      , Value . TString <$> lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))
      , Value (TBool True) <$ symbol "true"
      , Value (TBool False) <$ symbol "false"
      , Value TNil <$ symbol "nil"
      ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

atom :: Parser Expr
atom = choice [parens expr, literal, var]

expr :: Parser Expr
expr = makeExprParser atom operatorTable

binary :: T.Text -> (Expr -> Expr -> SourcePos -> Expr) -> Operator Parser Expr
binary name f = InfixL (flip3 f <$> (getSourcePos <* symbol name))

prefix :: T.Text -> (Expr -> SourcePos -> Expr) -> Operator Parser Expr
prefix name f = Prefix (flip f <$> (getSourcePos <* symbol name))

flip3 :: (a -> b -> c -> d) -> c -> a -> b -> d
flip3 f c a b = f a b c

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
    , prefix "+" const
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
