{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer.Class where

import Luna.Prelude
import GHC.Exts      (Any)
import Unsafe.Coerce (unsafeCoerce)
import Data.TypeDesc


--------------------
-- === Layers === --
--------------------

-- === Definition === --

data Layer = Layer deriving (Show)
type family LayerData l t

-- FIXME[WD]: refactor?
-- it is used as Term Definition in Model layer
type family Definition a


type LayerRep = TypeDescT Layer
