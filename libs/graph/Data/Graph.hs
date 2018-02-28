module Data.Graph where

import Prologue hiding (read)

import qualified Foreign.ForeignPtr.Utils as Ptr

import Foreign.ForeignPtr.Utils (ForeignPtr)
import Foreign.Storable         (Storable)
import System.IO.Unsafe         (unsafePerformIO)

import Control.Monad.State.Strict hiding (MonadIO, liftIO)
-- mallocForeignPtr :: Storable a => IO (ForeignPtr a)


import GHC.IORef



------------------
-- === Node === --
------------------

-- === Definition === --

newtype Node a = Node (ForeignPtr a)
makeLenses ''Node



-- === Construction === --

newNode :: (MonadIO m, Storable a) => a -> m (Node a)
newNode a = wrap <$> Ptr.mkForeignPtr a ; {-# INLINE newNode #-}



-- === Modification === --

read  :: (MonadIO m, Storable a) => Node a      -> m a
write :: (MonadIO m, Storable a) => Node a -> a -> m ()
read  n   = Ptr.readForeignPtr  (unwrap n)   ; {-# INLINE read  #-}
write n a = Ptr.writeForeignPtr (unwrap n) a ; {-# INLINE write #-}



-- === Instances === --

instance (Show a, Storable a) => Show (Node a) where
    show = show . unsafePerformIO . read ; {-# INLINE show #-}





-----------------------
-- === GraphView === --
-----------------------


-- data GraphView =
-- StateT
