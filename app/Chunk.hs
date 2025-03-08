{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <$>" #-}

module Chunk where

import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.ST
import Control.Monad.State
import Control.Monad.Writer
import Data.Bits (shiftL, shiftR)
import Data.Bool (bool)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List (find, findIndex)
import Data.Maybe (isNothing)
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import Data.Vector ((!))
import Data.Vector qualified as Vec
import Data.Vector.Mutable (STVector)
import Data.Vector.Mutable qualified as MVec
import Data.Word (Word16, Word8)
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
  | OpJumpIfFalse
  | OpJumpIfTrue
  | OpJump
  | OpPop
  | OpSetLocal
  | OpGetLocal
  | OpBindGlobal
  | OpSetGlobal
  | OpGetGlobal
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

data Local = Local
  { name :: String
  , depth :: Maybe Int
  }

-- TODO: replace with more haskell-y solution. scopeDepth and localCounts are totally unnecessary,
-- can be replaced with locals :: [Map String Int]
data CompilerState = CompilerState
  { constantCount :: Word8
  , scopeDepth :: Int
  , localCounts :: [Int]
  , locals :: [Local]
  }

type Compiler = ExceptT T.LoxError (State CompilerState)

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

readWord16 :: Int -> Chunk -> Word16
readWord16 idx chunk =
  let
    c = code chunk
    b1 = BS.index c idx
    b2 = BS.index c (idx + 1)
   in
    (fromIntegral b1 `shiftL` 8) + fromIntegral b2

getValue :: Int -> Chunk -> T.Value
getValue idx = (! idx) . constants

getLineNumber :: Int -> Chunk -> Int
getLineNumber idx = T.unPos . T.sourceLine . (! idx) . sourceInfo

getSourcePos :: Int -> Chunk -> T.SourcePos
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
      jumpInstruction' = jumpInstruction prefix idx True op <$ put (idx + 3)
      simpleInstruction = simple $ sformat (stext % stext) prefix (pack . show $ op)
  case op of
    OpConstant -> constantInstruction'
    OpBindGlobal -> constantInstruction'
    OpSetGlobal -> constantInstruction'
    OpGetGlobal -> constantInstruction'
    OpSetLocal -> localInstruction'
    OpGetLocal -> localInstruction'
    OpJumpIfFalse -> jumpInstruction'
    OpJumpIfTrue -> jumpInstruction'
    OpJump -> jumpInstruction'
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

chunkWithOperand :: SourcePos -> OpCode -> Word8 -> Chunk
chunkWithOperand l instruction operand =
  let code = BS.pack [toWord instruction, operand]
      constants = Vec.empty
      sourceInfo = Vec.fromList [l, l]
   in Chunk{..}

jumpChunk :: SourcePos -> OpCode -> Word16 -> Chunk
jumpChunk l instruction operand =
  let code = BS.pack [toWord instruction, fromIntegral $ operand `shiftR` 8, fromIntegral operand]
      constants = Vec.empty
      sourceInfo = Vec.fromList [l, l, l]
   in Chunk{..}

constantChunk :: SourcePos -> T.Value -> OpCode -> Compiler Chunk
constantChunk l v op = do
  idx <- gets constantCount
  modify (\c -> c{constantCount = idx + 1})
  let code = BS.pack [toWord op, idx]
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
fromOp f l op = basic (f op) l

basic :: OpCode -> SourcePos -> Chunk
basic op l = Chunk (BS.singleton (toWord op)) Vec.empty (Vec.singleton l)

defaultSourcePos :: SourcePos
defaultSourcePos = SourcePos "" (mkPos 1) (mkPos 1)

inNewScope :: Compiler Chunk -> Compiler Chunk
inNewScope m = do
  modify (\c -> c{scopeDepth = scopeDepth c + 1, localCounts = 0 : localCounts c})
  body <- m
  nLocals <- gets (head . localCounts)
  modify (\c -> c{scopeDepth = scopeDepth c - 1, localCounts = tail (localCounts c)})
  pure $ body <> mconcat (replicate nLocals (basic OpPop defaultSourcePos))

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb m = mb >>= flip when m

checkNotInScope :: String -> Compiler ()
checkNotInScope n = do
  ls <- gets locals
  case find ((== n) . name) ls of
    Just local ->
      whenM (gets ((depth local ==) . Just . scopeDepth)) $
        throwError (T.compilerError defaultSourcePos (T.Shadowing n))
    Nothing -> pure ()

declareLocal :: String -> Compiler Int
declareLocal name = do
  checkNotInScope name
  modify
    ( \c ->
        c
          { locals = Local name Nothing : locals c
          , localCounts = (head (localCounts c) + 1) : tail (localCounts c)
          }
    )
  gets (subtract 1 . length . locals)

defineLocal :: Compiler ()
defineLocal = do
  ls <- gets locals
  modify (\c -> c{locals = (head ls){depth = Just (scopeDepth c)} : tail ls})

resolveLocal :: String -> Compiler (Maybe Int)
resolveLocal n = do
  ls <- gets locals
  let res = resolve ls
  -- there's gotta be a better way
  case res of
    Nothing -> pure Nothing
    Just idx ->
      if isNothing (depth (ls !! idx))
        then
          throwError $ T.compilerError defaultSourcePos (T.SelfRefInDecl n)
        else pure (Just idx)
 where
  resolve ls = (length ls - 1 -) <$> findIndex ((== n) . name) ls

fromExpr :: T.Expr -> Compiler Chunk
fromExpr (T.Located l (T.BinOp op e1 e2)) = do
  c1 <- fromExpr e1
  c2 <- fromExpr e2
  let
    -- short circuit stuff
    -- 1 extra byte for pop instruction
    jumpLength = 1 + (fromIntegral . BS.length . code $ c2)
    popChunk = basic OpPop l
  pure $ case op of
    T.And -> c1 <> jumpChunk l OpJumpIfFalse jumpLength <> popChunk <> c2
    T.Or -> c1 <> jumpChunk l OpJumpIfTrue jumpLength <> popChunk <> c2
    _ -> c1 <> c2 <> fromOp fromBinOp l op
fromExpr (T.Located l (T.UnOp op e1)) = do
  c <- fromExpr e1
  pure (c <> fromOp fromUnOp l op)
fromExpr (T.Located l (T.Value v)) = constantChunk l v OpConstant
fromExpr (T.Located l (T.Ident s)) =
  resolveLocal s
    >>= \case
      Just offset -> pure $ chunkWithOperand l OpGetLocal (fromIntegral offset)
      Nothing -> constantChunk l (T.TString s) OpGetGlobal
fromExpr (T.Located l (T.Assgn s e)) =
  (<>)
    <$> fromExpr e
    <*> ( resolveLocal s
            >>= \case
              Just offset -> pure $ chunkWithOperand l OpSetLocal (fromIntegral offset)
              Nothing -> constantChunk l (T.TString s) OpSetGlobal
        )

fromDecl :: T.Decl -> Compiler Chunk
fromDecl (T.Bind s e) =
  gets scopeDepth
    >>= \case
      0 ->
        liftM2 (<>) (fromExpr e) (constantChunk (T.location e) (T.TString s) OpBindGlobal)
      _ -> do
        offset <- declareLocal s
        c <- fromExpr e
        (c <> chunkWithOperand (T.location e) OpSetLocal (fromIntegral offset))
          <$ defineLocal
fromDecl (T.EvalExpr e) = fmap (<> basic OpPop (T.location e)) (fromExpr e)
fromDecl (T.If cond trueBody falseBody) = do
  condChunk <- fromExpr cond
  trueChunk <- inNewScope $ fromProgram trueBody
  falseChunk <- case falseBody of
    Just body -> inNewScope $ fromProgram body
    Nothing -> pure mempty
  let popChunk = basic OpPop (T.location cond)
      -- extra 1 byte at start for pop instruction,
      -- 3 bytes at end for jump past false body
      trueJumpLength = 1 + (fromIntegral . BS.length . code $ trueChunk) + 3
      trueJumpChunk = jumpChunk (T.location cond) OpJumpIfFalse trueJumpLength
      -- extra 1 byte at start for pop instruction
      falseJumpLength = 1 + (fromIntegral . BS.length . code $ falseChunk)
      falseJumpChunk = jumpChunk (T.location cond) OpJump falseJumpLength
  pure . mconcat $
    [ condChunk
    , trueJumpChunk -- ──┐
    , popChunk --        │
    , trueChunk --       │
    , falseJumpChunk -- ─┼┐
    , popChunk -- ◄──────┘│
    , falseChunk --       │
    ] --  ◄───────────────┘

fromProgram :: T.Program -> Compiler Chunk
fromProgram = fmap mconcat . traverse fromDecl

fromDecl_ :: T.Decl -> Either T.LoxError Chunk
fromDecl_ d = evalState (runExceptT (fromDecl d)) (CompilerState 0 0 [] [])
