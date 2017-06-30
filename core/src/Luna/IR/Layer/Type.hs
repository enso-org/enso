{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Type where

import Luna.Prelude
import OCI.IR.Layer.Class

import OCI.IR.Class (SubLink)


-------------------------
-- === Type layer === --
-------------------------

data Type = Type deriving (Show)

type instance LayerData Type t = SubLink Type t
