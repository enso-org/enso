module Prologue.Placeholders_old (module Prologue.Placeholders_old, module X) where

import Prelude
import Language.Haskell.TH      (Q, Exp)
import Development.Placeholders as X

fixme :: String -> Q Exp
fixme msg = placeholder $ "FIXME: " ++ msg
