{-# LANGUAGE RecordWildCards #-}

module Compiler where

import AST qualified
import Chunk (Chunk (..), OpCode (..))
import Chunk qualified
import Control.Monad
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.State
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.Foldable
import Data.List (findIndex)
import Data.Maybe (isNothing)
import Data.Vector qualified as Vec
import Data.Word (Word16, Word8)
import Error
import Text.Megaparsec.Pos
import Types (Located (..))

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

type Compiler = ExceptT LoxError (State CompilerState)

compileASTValue :: AST.Value -> Chunk.Value
compileASTValue = \case
  AST.TNum x -> Chunk.Num x
  AST.TString s -> Chunk.String s
  AST.TNil -> Chunk.Nil
  AST.TBool b -> Chunk.Bool b
  AST.TFunction name ps body -> undefined

constantChunk :: SourcePos -> Chunk.Value -> Chunk.OpCode -> Compiler Chunk
constantChunk l v op = do
  idx <- gets constantCount
  modify (\c -> c{constantCount = idx + 1})
  let code = BS.pack [Chunk.toWord op, idx]
      constants = Vec.singleton v
      sourceInfo = Vec.fromList [l, l]
  pure Chunk{..}

binOp :: AST.BinOp -> OpCode
binOp = \case
  AST.Add -> OpAdd
  AST.Sub -> OpSub
  AST.Mul -> OpMul
  AST.Div -> OpDiv
  AST.Or -> OpOr
  AST.And -> OpAnd
  AST.Eq -> OpEq
  AST.Neq -> OpNeq
  AST.Leq -> OpLeq
  AST.Lt -> OpLt
  AST.Gt -> OpGt
  AST.Geq -> OpGeq

unOp :: AST.UnOp -> OpCode
unOp = \case
  AST.Negate -> OpNegate
  AST.Not -> OpNot

compileOp :: (op -> OpCode) -> SourcePos -> op -> Chunk
compileOp f l op = basic (f op) l

basic :: OpCode -> SourcePos -> Chunk
basic op l = Chunk (BS.singleton (Chunk.toWord op)) Vec.empty (Vec.singleton l)

defaultSourcePos :: SourcePos
defaultSourcePos = SourcePos "" (mkPos 1) (mkPos 1)

inNewScope :: Compiler Chunk -> Compiler Chunk
inNewScope m = do
  modify (\c -> c{scopeDepth = scopeDepth c + 1, localCounts = 0 : localCounts c})
  body <- m
  nLocals <- gets (head . localCounts)
  modify (\c -> c{scopeDepth = scopeDepth c - 1, localCounts = tail (localCounts c)})
  pure $
    mconcat (replicate nLocals (basic OpIncrStack defaultSourcePos))
      <> body
      <> mconcat (replicate nLocals (basic OpPop defaultSourcePos))

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb m = mb >>= flip when m

checkNotInScope :: String -> Compiler ()
checkNotInScope n = do
  ls <- gets locals
  case find ((== n) . name) ls of
    Just local ->
      whenM (gets ((depth local ==) . Just . scopeDepth)) $
        throwError (compilerError defaultSourcePos (Shadowing n))
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
          throwError $ compilerError defaultSourcePos (SelfRefInDecl n)
        else pure (Just idx)
 where
  resolve ls = (length ls - 1 -) <$> findIndex ((== n) . name) ls

jumpLength :: Chunk -> Word16
jumpLength = fromIntegral . BS.length . code

chunkWithOperand :: SourcePos -> OpCode -> Word8 -> Chunk
chunkWithOperand l instruction operand =
  let code = BS.pack [Chunk.toWord instruction, operand]
      constants = Vec.empty
      sourceInfo = Vec.fromList [l, l]
   in Chunk{..}

jumpChunk :: SourcePos -> OpCode -> Word16 -> Chunk
jumpChunk l instruction operand =
  let code = BS.pack [Chunk.toWord instruction, fromIntegral $ operand `shiftR` 8, fromIntegral operand]
      constants = Vec.empty
      sourceInfo = Vec.fromList [l, l, l]
   in Chunk{..}

compileExpr :: AST.Expr -> Compiler Chunk
compileExpr (Located l (AST.BinOp op e1 e2)) = do
  c1 <- compileExpr e1
  c2 <- compileExpr e2
  let
    -- short circuit stuff
    -- 1 extra byte for pop instruction
    jumpLength' = 1 + jumpLength c2
    popChunk = basic OpPop l
  pure $ case op of
    AST.And -> c1 <> jumpChunk l OpJumpIfFalse jumpLength' <> popChunk <> c2
    AST.Or -> c1 <> jumpChunk l OpJumpIfTrue jumpLength' <> popChunk <> c2
    _ -> c1 <> c2 <> compileOp binOp l op
compileExpr (Located l (AST.UnOp op e1)) = do
  c <- compileExpr e1
  pure (c <> compileOp unOp l op)
compileExpr (Located l (AST.Value v)) = constantChunk l (compileASTValue v) OpConstant
compileExpr (Located l (AST.Ident s)) =
  resolveLocal s
    >>= \case
      Just offset -> pure $ chunkWithOperand l OpGetLocal (fromIntegral offset)
      Nothing -> constantChunk l (Chunk.String s) OpGetGlobal
compileExpr (Located l (AST.Assgn s e)) =
  (<>)
    <$> compileExpr e
    <*> ( resolveLocal s
            >>= \case
              Just offset -> pure $ chunkWithOperand l OpSetLocal (fromIntegral offset)
              Nothing -> constantChunk l (Chunk.String s) OpSetGlobal
        )
compileExpr (Located l (AST.Call e es)) = do
  functionChunk <- compileExpr e
  paramsChunk <- mconcat <$> traverse compileExpr es
  let callChunk = chunkWithOperand l OpCall (fromIntegral . length $ es)
  pure $ functionChunk <> paramsChunk <> callChunk

compileStmt :: AST.Stmt -> Compiler Chunk
compileStmt (AST.EvalExpr e) = fmap (<> basic OpPop (location e)) (compileExpr e)
compileStmt (AST.If cond trueBody falseBody) = do
  condChunk <- compileExpr cond
  trueChunk <- compileStmt trueBody
  falseChunk <- case falseBody of
    Just body -> compileStmt body
    Nothing -> pure mempty
  let popChunk = basic OpPop (location cond)
      -- extra 1 byte at start for pop instruction,
      -- 3 bytes at end for jump past false body
      trueJumpLength = 1 + jumpLength trueChunk + 3
      trueJumpChunk = jumpChunk (location cond) OpJumpIfFalse trueJumpLength
      -- extra 1 byte at start for pop instruction
      falseJumpLength = 1 + jumpLength falseChunk
      falseJumpChunk = jumpChunk (location cond) OpJump falseJumpLength
  pure . mconcat $
    [ condChunk
    , trueJumpChunk -- ──┐
    , popChunk --        │
    , trueChunk --       │
    , falseJumpChunk -- ─┼┐
    , popChunk -- ◄──────┘│
    , falseChunk --       │
    ] --  ◄───────────────┘
compileStmt (AST.While cond body) = do
  condChunk <- compileExpr cond
  bodyChunk <- compileStmt body
  let popChunk = basic OpPop (location cond)
      loopJumpLength = jumpLength condChunk + 3 + 1 + jumpLength bodyChunk + 3
      loopChunk = jumpChunk (location cond) OpLoop loopJumpLength
      falseJumpLength = 1 + jumpLength bodyChunk + 3
      falseJumpChunk = jumpChunk (location cond) OpJumpIfFalse falseJumpLength
  pure . mconcat $
    [ condChunk -- ◄──────┐
    , falseJumpChunk -- ─┐│
    , popChunk --        ││
    , bodyChunk --       ││
    , loopChunk -- ──────┼┘
    , popChunk -- ◄──────┘
    ]
compileStmt (AST.Scope program) = inNewScope (compileProgram program)

compileDecl :: AST.Decl -> Compiler Chunk
compileDecl (AST.Bind s e) =
  gets scopeDepth
    >>= \case
      0 ->
        liftM2 (<>) (compileExpr e) (constantChunk (location e) (Chunk.String s) OpBindGlobal)
      _ -> do
        offset <- declareLocal s
        c <- compileExpr e
        (c <> chunkWithOperand (location e) OpSetLocal (fromIntegral offset) <> basic OpPop (location e))
          <$ defineLocal
compileDecl (AST.EvalStmt stmt) = compileStmt stmt

compileProgram :: AST.Program -> Compiler Chunk
compileProgram = fmap mconcat . traverse compileDecl

compileProgram_ :: AST.Program -> Either LoxError Chunk
compileProgram_ d = evalState (runExceptT (compileProgram d)) (CompilerState 0 0 [] [])
