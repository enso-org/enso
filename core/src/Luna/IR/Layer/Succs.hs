{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Succs where

import Luna.Prelude
import OCI.IR.Layer.Class
import OCI.IR.Layout.Class (Universal)
import OCI.IR.Class       (Link)

import Data.Set (Set)


-------------------------
-- === Succs layer === --
-------------------------

data Succs = Succs deriving (Show)

type instance LayerData Succs a = Set (Link a (Universal a))
