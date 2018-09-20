{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Syntax.Text.Parser.State.LastOffset where

import Data.Text.Position (Delta)
import Prologue



------------------------
-- === LastOffset === --
------------------------

newtype LastOffset = LastOffset Delta deriving (Show)
makeLenses ''LastOffset

instance Default LastOffset where def = LastOffset mempty
