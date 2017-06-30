module Data.Container.Mutable where

import Prologue

import qualified Data.Vector         as Vector
import           Data.Vector         (Vector)
import           Data.Vector.Mutable (MVector)


--------------------------------------------
-- === Mutable structures abstraction === --
--------------------------------------------

-- === Definitions === --

type family Mutable s t


-- === Mutable / Immutable conversions === --

class PrimMonad m => Freezable       m t where freeze       :: Mutable (PrimState m) t -> m t
class PrimMonad m => UnsafeFreezable m t where unsafeFreeze :: Mutable (PrimState m) t -> m t

class PrimMonad m => Thawable        m t where thaw         :: t -> m (Mutable (PrimState m) t)
class PrimMonad m => UnsafeThawable  m t where unsafeThaw   :: t -> m (Mutable (PrimState m) t)



--------------------------------
-- === Standard instances === --
--------------------------------

-- Vector
type instance Mutable s (Vector a) = MVector s a
instance PrimMonad m => Freezable       m (Vector a) where freeze       = Vector.freeze
instance PrimMonad m => UnsafeFreezable m (Vector a) where unsafeFreeze = Vector.unsafeFreeze
instance PrimMonad m => Thawable        m (Vector a) where thaw         = Vector.thaw
instance PrimMonad m => UnsafeThawable  m (Vector a) where unsafeThaw   = Vector.unsafeThaw
