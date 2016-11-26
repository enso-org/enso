module Luna.IR.Layer.Model where

import Luna.Prelude
import Luna.IR.Layer.Class


------------------------
-- === Model Layer === --
------------------------

data Model = Model deriving (Show)

type instance LayerData Model t = Definition t

instance Monad m => LayerCons Model m where
    consLayer = return . Layer ; {-# INLINE consLayer #-}
