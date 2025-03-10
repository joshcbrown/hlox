{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Chunk where

import AST qualified
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits (shiftL, shiftR)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Functor (($>))
import Data.List (findIndex)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import Data.Time.Clock.POSIX
import Data.Vector ((!))
import Data.Vector qualified as Vec
import Data.Word (Word16, Word8)
import Error
import Formatting
import Text.Megaparsec.Pos
import Types (Located (..))

type NativeFunction = [Value] -> SourcePos -> IO (Either LoxError Value)

instance Show NativeFunction where
  show _ = "<native function>"

data Value
  = Num Double
  | String String
  | Bool Bool
  | Nil
  | NativeFunction NativeFunction
  | Function String Int Chunk
  deriving (Show)

valEq :: Value -> Value -> Bool
valEq (Num x) (Num y) = x == y
valEq (Bool b1) (Bool b2) = b1 == b2
valEq (String s1) (String s2) = s1 == s2
valEq Nil Nil = True
valEq _ _ = False

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
  | OpJumpIfFalse
  | OpJumpIfTrue
  | OpJump
  | OpIncrStack
  | OpPop
  | OpSetLocal
  | OpGetLocal
  | OpBindGlobal
  | OpSetGlobal
  | OpGetGlobal
  | OpCall
  | OpReturn
  | OpLoop
  deriving (Enum, Show)

data Chunk = Chunk
  { code :: BS.ByteString
  , constants :: Vec.Vector Value
  , sourceInfo :: Vec.Vector SourcePos
  }
  deriving (Show)

instance Semigroup Chunk where
  (Chunk c11 c12 c13) <> (Chunk c21 c22 c23) = Chunk (c11 <> c21) (c12 <> c22) (c13 <> c23)

instance Monoid Chunk where
  mempty = Chunk{code = BS.empty, constants = Vec.empty, sourceInfo = Vec.empty}

valuePretty :: Value -> Text
valuePretty (Num x) = sformat (fixed 2) x
valuePretty (Bool b) = pack (show b)
valuePretty (String s) = pack s
valuePretty (NativeFunction _) = "native fun"
valuePretty (Function name _ _) = sformat ("function " % stext) (pack name)
valuePretty Nil = "nil"

toWord :: OpCode -> Word8
toWord = fromIntegral . fromEnum

fromWord :: Word8 -> OpCode
fromWord = toEnum . fromIntegral

readWord :: Int -> Chunk -> Word8
readWord idx = (`BS.index` idx) . code

readWord16 :: Int -> Chunk -> Word16
readWord16 idx chunk =
  let
    c = code chunk
    b1 = BS.index c idx
    b2 = BS.index c (idx + 1)
   in
    (fromIntegral b1 `shiftL` 8) + fromIntegral b2

getValue :: Int -> Chunk -> Value
getValue idx = (! idx) . constants

getLineNumber :: Int -> Chunk -> Int
getLineNumber idx = unPos . sourceLine . (! idx) . sourceInfo

getSourcePos :: Int -> Chunk -> SourcePos
getSourcePos idx = (! idx) . sourceInfo

instructionWithMeta :: (?chunk :: Chunk) => Text -> OpCode -> Text -> Text
instructionWithMeta prefix op =
  sformat
    (stext % right 16 ' ' % " " % stext)
    prefix
    (pack . show $ op)

constantInstruction :: (?chunk :: Chunk) => Text -> Int -> OpCode -> Text
constantInstruction prefix idx op =
  let valueIdx = fromIntegral $ readWord (idx + 1) ?chunk
      value = getValue valueIdx ?chunk
      meta =
        sformat
          (left 4 ' ' % " '" % stext % "'")
          valueIdx
          (valuePretty value)
   in instructionWithMeta prefix op meta

localInstruction :: (?chunk :: Chunk) => Text -> Int -> OpCode -> Text
localInstruction prefix idx op =
  let offset = readWord (idx + 1) ?chunk
      meta = sformat (left 4 ' ') offset
   in instructionWithMeta prefix op meta

jumpInstruction :: (?chunk :: Chunk) => Text -> Int -> Bool -> OpCode -> Text
jumpInstruction prefix idx positive op =
  let jump = fromIntegral $ readWord16 (idx + 1) ?chunk
      sign = bool (-1) 1 positive
      meta = sformat (left 4 ' ' % " -> " % int) idx (idx + 3 + sign * jump)
   in instructionWithMeta prefix op meta

disassembleInstruction :: (MonadState Int m, ?chunk :: Chunk) => m Text
disassembleInstruction = do
  idx <- get
  let op = fromWord . readWord idx $ ?chunk
      prefix = sformat (left 4 '0' %+ right 4 ' ') idx (getLineNumber idx ?chunk)
      constantInstruction' = constantInstruction prefix idx op <$ put (idx + 2)
      localInstruction' = localInstruction prefix idx op <$ put (idx + 2)
      jumpInstruction' pos = jumpInstruction prefix idx pos op <$ put (idx + 3)
      simpleInstruction = simple $ sformat (stext % stext) prefix (pack . show $ op)
  case op of
    OpConstant -> constantInstruction'
    OpBindGlobal -> constantInstruction'
    OpSetGlobal -> constantInstruction'
    OpGetGlobal -> constantInstruction'
    OpCall -> localInstruction'
    OpSetLocal -> localInstruction'
    OpGetLocal -> localInstruction'
    OpJumpIfFalse -> jumpInstruction' True
    OpJumpIfTrue -> jumpInstruction' True
    OpJump -> jumpInstruction' True
    OpLoop -> jumpInstruction' False
    _ -> simpleInstruction

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
