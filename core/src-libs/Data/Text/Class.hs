module Data.Text.Class where

import Prologue_old
import Control.Lens
import Data.Text

class HasText a where
    text :: Lens' a Text
