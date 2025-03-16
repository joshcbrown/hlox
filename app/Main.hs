module Main where

import AST qualified
import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.State
import BluefinHelpers
import Chunk
import Compiler (compileProgram_)
import Control.Monad (when)
import Data.Text qualified as T
import Error
import Opts (ExecutionMode (..), Options (..), executionMode, parseOptions)
import Parse (runLoxParser)
import VirtualMachine qualified as VM

runRepl :: Bool -> IO ()
runRepl debug = do
  initialState <- VM.initialState debug
  runEff $ \io -> do
    evalState initialState $ \state -> do
      runInput io $ \input -> repl state input io

repl :: (e1 :> es, e2 :> es, e3 :> es) => State VM.VMState e1 -> Input e2 -> IOE e3 -> Eff es ()
repl state input io = do
  minput <- readInputLine input "lox> "
  case minput of
    Nothing -> return ()
    Just "exit" -> return ()
    Just program -> do
      res <- try $ \exn -> executeProgram state exn io (T.pack program)
      case res of
        Left e -> effIO io $ print e
        _ -> pure ()
      VM.resetVM state
      repl state input io

whenDebug :: (e1 :> es) => State VM.VMState e1 -> Eff es () -> Eff es ()
whenDebug state e = gets state VM.debug >>= flip when e

compileWithDebug :: (e1 :> es, e2 :> es, e3 :> es) => State VM.VMState e1 -> Exception LoxError e2 -> IOE e3 -> AST.Program -> Eff es Chunk
compileWithDebug state exn io program =
  let (res, logs) = compileProgram_ program
   in do
        chunk <- liftEither exn res
        whenDebug state $ effIO io (putStrLn logs)
        whenDebug state $ effIO io (disassembleChunk chunk)
        pure chunk

executeProgram :: (e1 :> es, e2 :> es, e3 :> es) => State VM.VMState e1 -> Exception LoxError e2 -> IOE e3 -> T.Text -> Eff es ()
executeProgram state exn io input =
  liftEither exn (runLoxParser "" input)
    >>= compileWithDebug state exn io
    >>= VM.runProgram state exn io

regular :: Bool -> FilePath -> IO ()
regular debug file = do
  t <- T.pack <$> readFile file
  initialState <- VM.initialState debug
  res <- runEff $ \io ->
    evalState initialState $ \state ->
      try $ \exn ->
        executeProgram state exn io t
  case res of
    Left e -> print e
    Right () -> pure ()

main :: IO ()
main = do
  opts <- parseOptions
  case executionMode opts of
    Repl -> runRepl (debug opts)
    File file -> regular (debug opts) file
