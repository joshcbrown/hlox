module Chunk where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.ByteString qualified as BS
import Data.Foldable
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Vector ((!))
import Data.Vector qualified as Vec
import Data.Word (Word8)
import Formatting

data OpCode = OpConstant | OpNegate | OpAdd | OpSub | OpMul | OpDiv | OpReturn
  deriving (Enum, Show)

type Value = Double
data Chunk = Chunk
  { code :: BS.ByteString
  , constants :: Vec.Vector Value
  , lineNumbers :: Vec.Vector Int
  }
  deriving (Show)

valuePretty :: Value -> String
valuePretty = show

toWord :: OpCode -> Word8
toWord = fromIntegral . fromEnum

fromWord :: Word8 -> OpCode
fromWord = toEnum . fromIntegral

readWord :: Int -> Chunk -> Word8
readWord idx = (`BS.index` idx) . code

getValue :: Int -> Chunk -> Value
getValue idx = (! idx) . constants

getLineNumber :: Int -> Chunk -> Int
getLineNumber idx = (! idx) . lineNumbers

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
              (stext % right 16 ' ' % " " % left 4 ' ' % " '" % fixed 2 % "'")
              prefix
              ("OP_CONSTANT" :: Text)
              valueIdx
              value
       in finalString <$ put (idx + 2)
    OpReturn ->
      simple $ sformat (stext % "OP_RETURN") prefix
    OpNegate ->
      simple $ sformat (stext % "OP_NEGATE") prefix
    OpAdd ->
      simple $ sformat (stext % "OP_ADD") prefix
    OpSub ->
      simple $ sformat (stext % "OP_SUBTRACT") prefix
    OpMul ->
      simple $ sformat (stext % "OP_MULTIPLY") prefix
    OpDiv ->
      simple $ sformat (stext % "OP_DIVIDE") prefix

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
    , constants = Vec.fromList [1.2, 3.4, 5.6]
    , lineNumbers = Vec.fromList $ replicate 10 123
    }
