{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.Class where

import Luna.Prelude
import GHC.Prim      (Any)
import Unsafe.Coerce (unsafeCoerce)


--------------------
-- === Layers === --
--------------------

-- === Definition === --

type family LayerData l t

newtype     Layer  t l = Layer (LayerData l t)
type family Layers q a :: [*]

makeWrapped ''Layer

type family Definition a


-- === Isntances === --

deriving instance Show (Unwrapped (Layer t l))
      => Show (Layer t l)

instance Default (Unwrapped (Layer t l))
      => Default (Layer t l) where def = wrap' def ; {-# INLINE def #-}


-- === Construction === --

newtype AnyCons m = AnyCons (Any -> Any -> m Any)
makeWrapped ''AnyCons

type LayerCons  l t m = (t -> Definition t -> m (Layer t l))
type LayerCons' l t m = (t -> Definition t -> m (LayerData l t))
type LayerCons_   t m = (t -> Definition t -> m Any)

layerCons :: forall l t m . Monad m => LayerCons' l t m -> LayerCons l t m
layerCons = (fmap.fmap.fmap) Layer

anyCons :: forall l t m. Monad m => LayerCons' l t m -> AnyCons m
anyCons = wrap' . unsafeCoerce . layerCons @l ; {-# INLINE anyCons #-}

unsafeAppCons :: Functor m => AnyCons m -> LayerCons_ t m
unsafeAppCons = unsafeCoerce . unwrap' ; {-# INLINE unsafeAppCons #-}
