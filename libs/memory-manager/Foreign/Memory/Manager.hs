{-# LANGUAGE DeriveAnyClass           #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE UnliftedFFITypes         #-}
{-# LANGUAGE ViewPatterns             #-}

module Foreign.Memory.Manager where

import Prelude

import Control.DeepSeq        (NFData)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Convert           (convert)
import Foreign                (Ptr, castPtr, nullPtr)
import Foreign.C              (CDouble (..), CSize (..))
import GHC.Generics           (Generic)


---------------------------
-- === Type wrappers === --
---------------------------

newtype MemoryManager = MemoryManager (Ptr ()) deriving (Eq, Ord, Show, Generic, NFData)
type Item = Ptr ()



--------------------------------------
-- === Foreign imports from C++ === --
--------------------------------------

foreign import ccall unsafe "newManager"    c_newManager    :: CSize -> CSize -> IO MemoryManager
foreign import ccall unsafe "deleteManager" c_deleteManager :: MemoryManager -> IO ()
foreign import ccall unsafe "newItem"       c_newItem       :: MemoryManager -> IO Item
foreign import ccall unsafe "deleteItem"    c_deleteItem    :: MemoryManager -> Item -> IO ()



----------------------------------------
-- === Wrappers for foreign calls === --
----------------------------------------

newManager :: MonadIO m => Int -> Int -> m MemoryManager
newManager (convert -> blockSize) (convert -> itemSize) = liftIO $ c_newManager itemSize blockSize
{-# INLINE newManager #-}

deleteManager :: MonadIO m => MemoryManager -> m ()
deleteManager = liftIO . c_deleteManager
{-# INLINE deleteManager #-}

newItem :: MonadIO m => MemoryManager -> m (Ptr a)
newItem mm = liftIO $ castPtr <$> c_newItem mm
{-# INLINE newItem #-}

deleteItem :: MonadIO m => MemoryManager -> Ptr a -> m ()
deleteItem mm = liftIO . c_deleteItem mm . castPtr
{-# INLINE deleteItem #-}

unsafeNull :: MemoryManager
unsafeNull = MemoryManager nullPtr
