module Foreign.ForeignPtr.Utils where

import           Prelude
import           Foreign.ForeignPtr           (ForeignPtr, mallocForeignPtr, withForeignPtr)
import           Foreign.Storable             (Storable, peek, poke)
import           Control.Monad.IO.Class


mkForeignPtr :: (Storable a, MonadIO m) => a -> m (ForeignPtr a)
mkForeignPtr a = liftIO $ do
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr -> poke ptr a
    return fptr
{-# INLINE mkForeignPtr #-}

getAndMapForeignPtr :: (Storable a, MonadIO m) => ForeignPtr a -> (a -> a) -> m a
getAndMapForeignPtr fptr f = liftIO $ withForeignPtr fptr $ \ptr -> do
    val <- peek ptr
    poke ptr (f val)
    return val
{-# INLINE getAndMapForeignPtr #-}
