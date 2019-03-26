module Prologue.Unsafe (module Prologue.Unsafe, module X) where

import Prelude
import Prelude as X (error, undefined, head)

fromJustNote :: String -> Maybe a -> a
fromJustNote n = \case
    Just a  -> a
    Nothing -> error n
