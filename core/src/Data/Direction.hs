module Data.Direction where

import Prologue

import Control.Lens
import Old.Data.Prop


data Source = Source deriving (Show)
data Target = Target deriving (Show)



-- TODO: remove me (depreciated) vvv
class HasSource a where source :: Lens' a (a # Source)
class HasTarget a where target :: Lens' a (a # Target)



-- class HasSource a where source :: Lens' a (a ^. Source)
-- class HasTarget a where target :: Lens' a (a ^. Target)
--
--
-- class HasSourceM a m where readSource :: a -> m (a ^. Source)
--                            writeSource :: a ^. Source -> a -> m ()
--
-- class HasTargetM a m where readTarget :: a -> m (a ^. Target)
--                            writeTarget :: a ^. Target -> a -> m ()
