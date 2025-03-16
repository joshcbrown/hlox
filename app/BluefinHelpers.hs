module BluefinHelpers where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.IO
import System.Console.Haskeline

data Input e = MkInput
  { readInputLineImpl :: forall e'. String -> Eff (e' :& e) (Maybe String)
  }

instance Handle Input where
  mapHandle input =
    MkInput{readInputLineImpl = \s -> useImplUnder (readInputLineImpl input s)}

readInputLine :: (e :> es) => Input e -> String -> Eff es (Maybe String)
readInputLine input s = makeOp (readInputLineImpl (mapHandle input) s)

foo :: String -> IO (Maybe String)
foo = runInputT settings . getInputLine
 where
  settings =
    defaultSettings
      { historyFile = Just ".lox_history"
      }

runInput ::
  forall e1 es r.
  (e1 :> es) =>
  IOE e1 ->
  (forall e. Input e -> Eff (e :& es) r) ->
  Eff es r
runInput io k =
  useImplIn
    k
    MkInput
      { readInputLineImpl = \s -> effIO io (foo s)
      }
