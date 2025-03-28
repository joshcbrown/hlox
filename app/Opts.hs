module Opts (
  Options (..),
  ExecutionMode (..),
  parseOptions,
) where

-- this is claude generated, i cbs writing my own

import Options.Applicative

data ExecutionMode
  = Repl
  | File FilePath
  deriving (Show, Eq)

data Options = Options
  { executionMode :: ExecutionMode
  , debug :: Bool
  }
  deriving (Show, Eq)

modeParser :: Parser ExecutionMode
modeParser = fileMode <|> pure Repl
 where
  fileMode =
    File
      <$> argument
        str
        ( metavar "FILE"
            <> help "Source file to execute (omit to run REPL)"
        )

optionsParser :: Parser Options
optionsParser =
  Options
    <$> modeParser
    <*> switch
      ( long "debug"
          <> short 'd'
          <> help "show disassembly of instruction before execution"
      )

parseOptions :: IO Options
parseOptions = execParser opts
 where
  opts =
    info
      (optionsParser <**> helper)
      ( fullDesc
          <> progDesc "Execute source files or run an interactive REPL"
          <> header "language-exe - a programming language interpreter"
      )
