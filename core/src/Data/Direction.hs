module Data.Direction where

import Prologue

import Control.Lens
import Old.Data.Prop


data Source = Source deriving (Show)
data Target = Target deriving (Show)



-- TODO: remove me (depreciated) vvv
class HasSource a where source :: Lens' a (a # Source)
class HasTarget a where target :: Lens' a (a # Target)
