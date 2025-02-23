module VirtualMachine where

import Data.Vector qualified as Vec

data VMState = VMState
  { -- in the book, this is represented as just a ByteString, but i'm not sure that's practical...
    currentInstruction :: Int
  , stack :: Vec.Vector Double
  }
