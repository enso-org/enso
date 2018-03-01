{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Foreign.Memory.Manager where

import Prelude

import Control.Monad.IO.Class (MonadIO, liftIO)
import Foreign (Ptr, castPtr)
import Foreign.C (CDouble(..), CSize(..))
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)


---------------------------
-- === Type wrappers === --
---------------------------

newtype MemoryManager = MemoryManager (Ptr ()) deriving (Eq, Ord, Show, Generic, NFData)
type Item = Ptr ()



--------------------------------------
-- === Foreign imports from C++ === --
--------------------------------------

foreign import ccall unsafe "newManager"    c_newManager    :: CSize -> IO MemoryManager
foreign import ccall unsafe "deleteManager" c_deleteManager :: MemoryManager -> IO ()
foreign import ccall unsafe "newItem"       c_newItem       :: MemoryManager -> IO Item
foreign import ccall unsafe "deleteItem"    c_deleteItem    :: MemoryManager -> Item -> IO ()



----------------------------------------
-- === Wrappers for foreign calls === --
----------------------------------------

newManager :: MonadIO m => Int -> m MemoryManager
newManager = liftIO . c_newManager . fromIntegral
{-# INLINE newManager #-}

deleteManager :: MonadIO m => MemoryManager -> m ()
deleteManager = liftIO . c_deleteManager
{-# INLINE deleteManager #-}

newItem :: MonadIO m => MemoryManager -> m (Ptr a)
newItem mm = liftIO $ castPtr <$> c_newItem mm
{-# INLINE newItem #-}

deleteItem :: MonadIO m => MemoryManager -> Ptr a -> m ()
deleteItem mm = liftIO . c_deleteItem mm . castPtr
