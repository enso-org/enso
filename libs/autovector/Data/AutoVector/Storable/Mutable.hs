{-# LANGUAGE Strict #-}

module Data.AutoVector.Storable.Mutable where

import           Prelude                      hiding (length, (.))

import           Control.Exception.Base (assert)
import           Control.Lens.Utils
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Convert
import qualified Data.Vector.Storable         as Storable
import qualified Data.Vector.Storable         as Vector hiding (length)
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable.Mutable as Storable
import qualified Data.Vector.Storable.Mutable as Vector
import           Data.Vector.Storable.Mutable (MVector)
import           Foreign.Storable             (Storable)
import           GHC.Exts                     (fromList)
import           GHC.Generics                 (Generic)

import qualified Foreign.ForeignPtr           as Ptr
import           Foreign.ForeignPtr           (ForeignPtr)
import           Foreign.ForeignPtr.Utils     (mkForeignPtr, getAndMapForeignPtr, mapAndGetForeignPtr)


import Control.Monad.IO.Class

import Control.DeepSeq (NFData, rnf)
import Control.Monad (when)



-------------------------
-- === MAutoVector === --
-------------------------

-- === Definition === --

type MAutoVector' m   = MAutoVector (PrimState m)
data MAutoVector  s a = MAutoVector
    { _vector  :: {-# UNPACK #-} !(MVector s a)
    , _freeIxs :: {-# UNPACK #-} !(MVector s Int)
    , _counter :: ForeignPtr Int
    } deriving (Generic)
makeLenses ''MAutoVector



-- === Properties === --

-- | O(1)
length :: Storable a => MAutoVector s a -> Int
length s = Vector.length $ s ^. vector ; {-# INLINE length #-}



-- === Construction === --

alloc :: (MonadIO m, PrimMonad m, Storable a) => Int -> m (MAutoVector' m a)
alloc !i = MAutoVector <$> Vector.unsafeNew i
                       <*> Vector.unsafeThaw (Vector.generate i (maxKey-))
                       <*> mkForeignPtr maxKey
    where maxKey = i - 1
{-# INLINE alloc #-}



-- === Modifications === --

-- | O(1) / O(1) + memcpy
reserveKey :: (MonadIO m, PrimMonad m) => MAutoVector' m a -> m Int
reserveKey v = do
    kid <- getAndMapForeignPtr (v ^. counter) (subtract 1)
    when (kid < 0) $ error "TODO: resize"
    Vector.unsafeRead (v ^. freeIxs) kid
{-# INLINE reserveKey #-}

-- | O(1)
releaseKey :: (MonadIO m, PrimMonad m) => MAutoVector' m a -> Int -> m ()
releaseKey v i = do
    kid <- mapAndGetForeignPtr (v ^. counter) (+1)
    Vector.unsafeWrite (v ^. freeIxs) kid i
{-# INLINE releaseKey #-}


--
-- unsafeGrow :: (PrimMonad m, Storable a) => MAutoVector' m a -> Int -> m (MAutoVector' m a)
-- unsafeGrow !s !i = assert (i > 0) $ vector (flip Vector.unsafeGrow i) s ; {-# INLINE unsafeGrow #-} -- FIXME: alokacja pustych adresow!
--
-- unsafeDoubleGrow :: (PrimMonad m, Storable a) => MAutoVector' m a -> m (MAutoVector' m a)
-- unsafeDoubleGrow !s = unsafeGrow s (length s) ; {-# INLINE unsafeDoubleGrow #-}
--
-- unsafeWrite :: (PrimMonad m, Storable a, Convertible' k Int) => MAutoVector' m a -> k -> a -> m ()
-- unsafeWrite !s !k !a = Vector.unsafeWrite (s ^. vector) (convert' k) a ; {-# INLINE unsafeWrite #-}
--
-- unsafeWriteSpec :: forall t a k m. (PrimMonad m, Storable t, Convertible' k Int, t~a) => MAutoVector' m a -> k -> t -> m ()
-- unsafeWriteSpec !s !k !t = Vector.unsafeWrite (s ^. vector) (convert' k) t ; {-# INLINE unsafeWriteSpec #-}



-- === Instances === --

instance NFData (MAutoVector s a) where
    rnf v = () where
        !_ = rnf (v ^. vector)
        !_ = rnf (v ^. freeIxs)
