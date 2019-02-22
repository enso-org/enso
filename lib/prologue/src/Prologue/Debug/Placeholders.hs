module Prologue.Debug.Placeholders where

import Prelude
import Language.Haskell.TH (Q, Exp)
import qualified Development.Placeholders as P

_NOT_IMPLEMENTED :: Q Exp
_TODO, _FIXME    :: String -> Q Exp
_NOT_IMPLEMENTED = P.notImplemented
_TODO            = P.todo
_FIXME msg       = P.placeholder $ "FIXME: " ++ msg
