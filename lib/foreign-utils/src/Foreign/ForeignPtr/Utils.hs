module Foreign.ForeignPtr.Utils (module Foreign.ForeignPtr.Utils, module X) where

import Foreign.ForeignPtr as X
import Prelude

import Control.Monad.IO.Class
import Foreign.ForeignPtr     (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Storable       (Storable, peek, poke)


type SomeForeignPtr = ForeignPtr ()


mkForeignPtr :: (Storable a, MonadIO m) => a -> m (ForeignPtr a)
mkForeignPtr a = do
    ptr <- liftIO mallocForeignPtr
    writeForeignPtr ptr a
    return ptr
{-# INLINE mkForeignPtr #-}

readForeignPtr  :: (Storable a, MonadIO m) => ForeignPtr a -> m a
writeForeignPtr :: (Storable a, MonadIO m) => ForeignPtr a -> a -> m ()
readForeignPtr  fptr   = liftIO $ withForeignPtr fptr $ \ptr -> peek ptr   ; {-# INLINE readForeignPtr  #-}
writeForeignPtr fptr a = liftIO $ withForeignPtr fptr $ \ptr -> poke ptr a ; {-# INLINE writeForeignPtr #-}

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

