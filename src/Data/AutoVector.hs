{-# LANGUAGE UndecidableInstances #-}

module Data.AutoVector where

import Control.DeepSeq          (NFData)
import Control.Lens.Utils
import Data.Default
import Data.Container           (Item, Container, Tup2RTup, IsContainerM(fromContainerM), HasContainerM(setContainerM, viewContainerM))
import Data.Container.Auto
import Data.Container.List
import Data.Container.Resizable (Exponential)
import Data.Layer_OLD
import Data.Vector              (Vector)
import Data.Vector.Mutable      (MVector)
import GHC.Generics             (Generic)
import Control.Monad.Primitive  (PrimMonad, PrimState)

import qualified Data.Vector as V


-- === Definitions === --

newtype  AutoVector   a =  AutoVector (Auto Exponential (Vector a))    deriving (Generic, Show, Default)
newtype MAutoVector s a = MAutoVector (Auto Exponential (MVector s a)) deriving (Generic)
makeWrapped ''AutoVector
makeWrapped ''MAutoVector


-- === Utils === --

-- TODO[WD]: Move to containers as container utility function
unsafeThaw :: PrimMonad m => AutoVector a -> m (MAutoVector (PrimState m) a)
unsafeThaw = wrappedM' $ mapM V.unsafeThaw ; {-# INLINE unsafeThaw #-}

-- TODO[WD]: Move to containers as container utility function
unsafeFreeze :: PrimMonad m => MAutoVector (PrimState m) a -> m (AutoVector a)
unsafeFreeze = wrappedM' $ mapM V.unsafeFreeze ; {-# INLINE unsafeFreeze #-}


-- === Instances === --

-- Normal Form

instance (NFData a, Tup2RTup a ~ (a, ())) => NFData (AutoVector a)

-- Wrappers

type instance Unlayered ( AutoVector   a) = Unwrapped ( AutoVector   a)
type instance Unlayered (MAutoVector s a) = Unwrapped (MAutoVector s a)
instance      Layered   ( AutoVector   a)
instance      Layered   (MAutoVector s a)

-- List conversions

type     instance Item     (AutoVector a) = Item (Unwrapped (AutoVector a))
deriving instance ToList   (AutoVector a)
deriving instance FromList (AutoVector a)

-- Containers

type instance Container ( AutoVector   a) = Container (Unwrapped ( AutoVector   a))
type instance Container (MAutoVector s a) = Container (Unwrapped (MAutoVector s a))

instance Monad m => HasContainerM m (AutoVector    a) where viewContainerM = viewContainerM . unwrap ; {-# INLINE viewContainerM #-}
                                                            setContainerM  = wrapped . setContainerM ; {-# INLINE setContainerM  #-}

instance Monad m => HasContainerM m (MAutoVector s a) where viewContainerM = viewContainerM . unwrap ; {-# INLINE viewContainerM #-}
                                                            setContainerM  = wrapped . setContainerM ; {-# INLINE setContainerM  #-}

instance Monad m => IsContainerM  m (AutoVector    a) where fromContainerM = fmap  AutoVector . fromContainerM ; {-# INLINE fromContainerM #-}
instance Monad m => IsContainerM  m (MAutoVector s a) where fromContainerM = fmap MAutoVector . fromContainerM ; {-# INLINE fromContainerM #-}
