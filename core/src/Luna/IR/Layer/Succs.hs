module Luna.IR.Layer.Succs where

import Luna.Prelude
import Luna.IR.Layer.Class
import Luna.IR.Term.Layout.Class (Universal)

import Data.Set (Set)


-------------------------
-- === Succs layer === --
-------------------------

data Succs = Succs deriving (Show)

type instance LayerData Succs a = Set (Universal a)

instance Monad m => LayerCons Succs m where
    consLayer _ _ = return def ; {-# INLINE consLayer #-}
