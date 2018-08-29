{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Luna.IR.Term.Instances where

import Prologue

import qualified Data.Graph.Component.Node.Construction as Term

import Luna.IR.Term.Core (Top, top)


------------------------------
-- === Orphan instances === --
------------------------------

-- | We are plumbing Top as a default type constructor. It is used when
--   constructing a new term and we break with it circular dependencies.
instance Term.Creator Top m => Term.DefaultType m where
    defaultType = coerce <$> top ; {-# INLINE defaultType #-}

