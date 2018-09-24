{-# OPTIONS_GHC -Wno-orphans #-}

module OCI.Data.Name.Instances where

import Prologue

import           FastString (FastString)
import qualified FastString as FastString



------------------------
-- === FastString === --
------------------------

-- === Missing instances === --

instance Convertible String     FastString where convert = fromString          ; {-# INLINE convert #-}
instance Convertible FastString String     where convert = FastString.unpackFS ; {-# INLINE convert #-}

