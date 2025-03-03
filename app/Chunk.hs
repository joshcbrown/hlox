{-# LANGUAGE RecordWildCards #-}

module Chunk where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import Data.Vector ((!))
import Data.Vector qualified as Vec
import Data.Word (Word8)
import Formatting
import GHC.ExecutionStack (Location (functionName))
import Parse
import Text.Megaparsec.Pos
import Types qualified as T

data OpCode
  = OpConstant
  | OpNegate
  | OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpNot
  | OpOr
  | OpAnd
  | OpEq
  | OpNeq
  | OpLt
  | OpLeq
  | OpGt
  | OpGeq
  | OpReturn
  deriving (Enum, Show)

data Chunk = Chunk
  { code :: BS.ByteString
  , constants :: Vec.Vector T.Value
  , sourceInfo :: Vec.Vector T.SourcePos
  }
  deriving (Show)

instance Semigroup Chunk where
  (Chunk c11 c12 c13) <> (Chunk c21 c22 c23) = Chunk (c11 <> c21) (c12 <> c22) (c13 <> c23)

instance Monoid Chunk where
  mempty = Chunk{code = BS.empty, constants = Vec.empty, sourceInfo = Vec.empty}

valuePretty :: T.Value -> Text
valuePretty (T.TNum x) = sformat (fixed 2) x
valuePretty (T.TBool b) = pack (show b)
valuePretty (T.TString s) = pack s
valuePretty (T.TNativeFunction _) = "native fun"
valuePretty (T.TFunction name _ _) = sformat ("function " % stext) (pack name)
valuePretty T.TNil = "nil"

toWord :: OpCode -> Word8
toWord = fromIntegral . fromEnum

fromWord :: Word8 -> OpCode
fromWord = toEnum . fromIntegral

readWord :: Int -> Chunk -> Word8
readWord idx = (`BS.index` idx) . code

getValue :: Int -> Chunk -> T.Value
getValue idx = (! idx) . constants

getLineNumber :: Int -> Chunk -> Int
getLineNumber idx = T.unPos . T.sourceLine . (! idx) . sourceInfo

getSourcePos :: Int -> Chunk -> T.SourcePos
getSourcePos idx = (! idx) . sourceInfo

disassembleInstruction :: (MonadState Int m, ?chunk :: Chunk) => m Text
disassembleInstruction = do
  idx <- get
  let op = fromWord . readWord idx $ ?chunk
      prefix = sformat (left 4 '0' % " " % right 4 ' ') idx (getLineNumber idx ?chunk)
  case op of
    OpConstant ->
      let valueIdx = fromIntegral $ readWord (idx + 1) ?chunk
          value = getValue valueIdx ?chunk
          finalString =
            sformat
              (stext % right 16 ' ' % " " % left 4 ' ' % " '" % stext % "'")
              prefix
              (pack . show $ op)
              valueIdx
              (valuePretty value)
       in finalString <$ put (idx + 2)
    _ -> simple $ sformat (stext % stext) prefix (pack . show $ op)

simple :: (MonadState Int m) => Text -> m Text
simple t = t <$ modify (+ 1)

disassembleInstruction_ :: Int -> Chunk -> IO ()
disassembleInstruction_ offset chunk =
  let ?chunk = chunk
   in TIO.putStrLn $ evalState disassembleInstruction offset

disassembleChunk :: Chunk -> IO ()
disassembleChunk chunk =
  let ((), logs) = let ?chunk = chunk in runWriter (evalStateT disassembleLoop 0)
   in traverse_ TIO.putStrLn logs

disassembleLoop :: (MonadState Int m, MonadWriter [Text] m, ?chunk :: Chunk) => m ()
disassembleLoop = do
  idx <- get
  when (idx < BS.length (code ?chunk)) $ do
    result <- disassembleInstruction
    tell [result]
    disassembleLoop

exChunk :: Chunk
exChunk =
  Chunk
    { code =
        BS.pack
          [ toWord OpConstant
          , 0
          , toWord OpConstant
          , 1
          , toWord OpAdd
          , toWord OpConstant
          , 2
          , toWord OpDiv
          , toWord OpNegate
          , toWord OpReturn
          ]
    , constants = Vec.fromList [T.TNum 1.2, T.TNum 3.4, T.TNum 5.6]
    , sourceInfo = Vec.fromList $ replicate 10 (T.SourcePos "foo" (mkPos 10) (mkPos 10))
    }

fromValue :: (MonadState Word8 m) => SourcePos -> T.Value -> m Chunk
fromValue l v = do
  idx <- get
  modify (+ 1)
  let code = BS.pack [toWord OpConstant, idx]
      constants = Vec.singleton v
      sourceInfo = Vec.fromList [l, l]
  pure Chunk{..}

fromBinOp :: T.BinOp -> OpCode
fromBinOp = \case
  T.Add -> OpAdd
  T.Sub -> OpSub
  T.Mul -> OpMul
  T.Div -> OpDiv
  T.Or -> OpOr
  T.And -> OpAnd
  T.Eq -> OpEq
  T.Neq -> OpNeq
  T.Leq -> OpLeq
  T.Lt -> OpLt
  T.Gt -> OpGt
  T.Geq -> OpGeq

-- data BinOp = Dot | Add | Sub | Mul | Div | Or | And | Eq | Neq | Lt | Leq | Gt | Geq

fromUnOp :: T.UnOp -> OpCode
fromUnOp = \case
  T.Negate -> OpNegate
  T.Not -> OpNot

fromOp :: (op -> OpCode) -> SourcePos -> op -> Chunk
fromOp f l op =
  let instruction = toWord . f $ op
   in Chunk (BS.singleton instruction) Vec.empty (Vec.singleton l)

fromExpr :: (MonadState Word8 m) => T.Expr -> m Chunk
fromExpr (T.Located l (T.BinOp op e1 e2)) = do
  c1 <- fromExpr e1
  c2 <- fromExpr e2
  pure (c1 <> c2 <> fromOp fromBinOp l op)
fromExpr (T.Located l (T.UnOp op e1)) = do
  c <- fromExpr e1
  pure (c <> fromOp fromUnOp l op)
fromExpr (T.Located l (T.Value v)) = fromValue l v

-- TODO: remove
terminal :: Chunk
terminal = Chunk (BS.singleton (toWord OpReturn)) Vec.empty (Vec.singleton (SourcePos "" (mkPos 1) (mkPos 1)))

fromExpr_ :: T.Expr -> Chunk
fromExpr_ e = evalState (fromExpr e) 0 <> terminal
