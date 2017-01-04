{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Class where

import Luna.Prelude
import GHC.Prim      (Any)
import Unsafe.Coerce (unsafeCoerce)


--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Layer = Layer deriving (Show)
type family LayerData l t

-- FIXME[WD]: refactor?
type family Definition a
