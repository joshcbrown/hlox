module Main where

import Bluefin.Eff
import Bluefin.Exception
import Bluefin.IO
import Bluefin.State
import BluefinHelpers
import Chunk
import Compiler (compileProgram_)
import Data.Text qualified as T
import Error
import Opts (ExecutionMode (..), executionMode, parseOptions)
import Parse (runLoxParser)
import VirtualMachine qualified as VM

runRepl :: IO ()
runRepl = do
  initialState <- VM.initialState
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
      res <- try $ \exn -> executeRepl state exn io (T.pack program)
      case res of
        Left e -> effIO io $ print e
        _ -> pure ()
      VM.resetVM state
      repl state input io

executeRepl :: (e1 :> es, e2 :> es, e3 :> es) => State VM.VMState e1 -> Exception LoxError e2 -> IOE e3 -> T.Text -> Eff es ()
executeRepl state exn io input =
  liftEither (runLoxParser "" input) >>= \d -> do
    c <- liftEither $ compileProgram_ d
    effIO io $ disassembleChunk c
    VM.runProgram state exn io c
 where
  liftEither = either (throw exn) pure

executeProgram :: (e1 :> es, e2 :> es, e3 :> es) => State VM.VMState e1 -> Exception LoxError e2 -> IOE e3 -> T.Text -> Eff es ()
executeProgram state exn io input =
  liftEither (runLoxParser "" input)
    >>= liftEither . compileProgram_
    >>= VM.runProgram state exn io
 where
  liftEither = either (throw exn) pure

regular :: FilePath -> IO ()
regular file = do
  t <- T.pack <$> readFile file
  initialState <- VM.initialState
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
    Repl -> runRepl
    File file -> regular file
