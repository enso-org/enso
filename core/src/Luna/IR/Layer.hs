{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer where

import Luna.Prelude
import Luna.IR.Layer.Model


--------------------
-- === Layers === --
--------------------

-- === Definition === --

type family LayerData l t

newtype     Layer  t l = Layer (LayerData l t)
type family Layers q a :: [*]

makeWrapped ''Layer


-- === Classes === --

class Monad m => LayerCons l m where
    consLayer :: forall t. LayerData Model t -> m (Layer t l)


-- === Isntances === --

deriving instance Show (Unwrapped (Layer t l))
      => Show (Layer t l)

instance Default (Unwrapped (Layer t l))
      => Default (Layer t l) where def = wrap' def ; {-# INLINE def #-}
