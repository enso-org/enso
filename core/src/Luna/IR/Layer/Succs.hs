{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Succs where

import Luna.Prelude
import Luna.IR.Layer.Class
import Luna.IR.Expr.Layout.Class (Universal)
import Luna.IR.Internal.IR       (Link)

import Data.Set (Set)


-------------------------
-- === Succs layer === --
-------------------------

data Succs = Succs deriving (Show)

type instance LayerData Succs a = Set (Link a (Universal a))
