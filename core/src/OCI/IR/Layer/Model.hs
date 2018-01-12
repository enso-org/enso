{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer.Model where

import Luna.Prelude
import GHC.Exts      (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.TypeDesc
import OCI.IR.Layer.Class


-------------------
-- === Model === --
-------------------

-- We may want to move it somewhere else after removing Model dependencies from IR.Internal.IR

data Model = Model deriving (Show)

type instance LayerData Model t = Definition t
