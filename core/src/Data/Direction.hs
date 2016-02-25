module Data.Direction where

import Prologue

import Control.Lens
import Data.Prop


data Source = Source deriving (Show)
data Target = Target deriving (Show)

class HasSource a where source :: Lens' a (a # Source)
class HasTarget a where target :: Lens' a (a # Target)

