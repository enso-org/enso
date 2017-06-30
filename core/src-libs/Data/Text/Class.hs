module Data.Text.Class where

import Prologue
import Control.Lens
import Data.Text

class HasText a where
    text :: Lens' a Text
