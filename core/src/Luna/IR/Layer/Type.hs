{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Type where

import Luna.Prelude
import Luna.IR.Layer.Class

import Luna.IR.Internal.IR (SubLink)


-------------------------
-- === Type layer === --
-------------------------

data Type = Type deriving (Show)

type instance LayerData Type t = SubLink Type t
