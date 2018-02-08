{-# LANGUAGE Strict #-}

module Foreign.ForeignPtr.Utils where

import           Prelude
import           Foreign.ForeignPtr           (ForeignPtr, mallocForeignPtr, withForeignPtr)
import           Foreign.Storable             (Storable, peek, poke)
import           Control.Monad.IO.Class


mkForeignPtr :: (Storable a, MonadIO m) => a -> m (ForeignPtr a)
mkForeignPtr a = do
    ptr <- liftIO $ mallocForeignPtr
    setForeignPtr ptr a
    return ptr
{-# INLINE mkForeignPtr #-}

setForeignPtr :: (Storable a, MonadIO m) => ForeignPtr a -> a -> m ()
setForeignPtr fptr a = liftIO $ withForeignPtr fptr $ \ptr -> poke ptr a ; {-# INLINE setForeignPtr #-}

getAndMapandGetForeignPtr :: (Storable a, MonadIO m) => ForeignPtr a -> (a -> a) -> m (a,a)
getAndMapForeignPtr       :: (Storable a, MonadIO m) => ForeignPtr a -> (a -> a) -> m a
mapAndGetForeignPtr       :: (Storable a, MonadIO m) => ForeignPtr a -> (a -> a) -> m a
getAndMapForeignPtr       fptr f = fst <$> getAndMapandGetForeignPtr fptr f ; {-# INLINE getAndMapForeignPtr #-}
mapAndGetForeignPtr       fptr f = snd <$> getAndMapandGetForeignPtr fptr f ; {-# INLINE mapAndGetForeignPtr #-}
getAndMapandGetForeignPtr fptr f = liftIO $ withForeignPtr fptr $ \ptr -> do
    val <- peek ptr
    let nval = f val
    poke ptr nval
    return (val,nval)
{-# INLINE getAndMapandGetForeignPtr #-}

mapForeignPtr :: (Storable a, MonadIO m) => ForeignPtr a -> (a -> a) -> m ()
mapForeignPtr fptr f = () <$ getAndMapForeignPtr fptr f ; {-# INLINE mapForeignPtr #-}
