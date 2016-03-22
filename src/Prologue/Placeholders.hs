module Prologue.Placeholders (module Prologue.Placeholders, module X) where

import Prelude
import Language.Haskell.TH      (Q, Exp)
import Development.Placeholders as X

fixme :: String -> Q Exp
fixme msg = placeholder $ "FIXME: " ++ msg
