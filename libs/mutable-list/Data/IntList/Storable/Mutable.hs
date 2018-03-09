module Data.IntList.Storable.Mutable where

import Control.Monad.IO.Class
import Foreign.ForeignPtr.Utils hiding (newForeignPtr)
import Foreign.Storable.Utils
import Prelude

import           Foreign               (ForeignPtr, FunPtr, Ptr, castPtr,
                                        nullPtr)
import           Foreign.Concurrent    (newForeignPtr)
import qualified Foreign.Marshal.Alloc as Ptr
import qualified Foreign.Marshal.Utils as Ptr
import           System.IO.Unsafe      (unsafePerformIO)

import Data.Monoid
-------------------
-- === IntListM === --
-------------------

-- === Definition === --

newtype IntListM = IntListM (ForeignPtr IntListMData)

data IntListMData
  = Null
  | Cons {-# UNPACK #-} !Int
         {- #UNPACK #-} !(Ptr IntListMData)


-- === Utils === --

new :: MonadIO m => m IntListM
new = fromIntListMData Null

fromIntListMData :: MonadIO m => IntListMData -> m IntListM
fromIntListMData a = liftIO $ do
    ptr <- Ptr.new a
    IntListM <$> newForeignPtr ptr (listFinalizer ptr)
{-# INLINE fromIntListMData #-}

unshift :: MonadIO m => IntListM -> Int -> m ()
unshift (IntListM fptr) i = liftIO $ withForeignPtr fptr $ \ptr -> do
    lst  <- peek ptr
    nptr <- Ptr.new lst
    poke ptr $ Cons i nptr
{-# INLINE unshift #-}

shift :: MonadIO m => IntListM -> m (Maybe Int)
shift (IntListM fptr) = liftIO $ withForeignPtr fptr $ \ptr -> do
    peek ptr >>= \case
      Null -> return Nothing
      Cons i tailPtr -> do
          Ptr.copyBytes ptr tailPtr (sizeOf' @IntListMData)
          Ptr.free tailPtr
          return $ Just i
{-# INLINE shift #-}

head :: MonadIO m => IntListM -> m (Maybe Int)
head (IntListM fptr) = liftIO $ withForeignPtr fptr $ \ptr -> do
    peek ptr >>= \case
      Null -> return Nothing
      Cons i _ -> return $ Just i
{-# INLINE head #-}

toList :: MonadIO m => IntListM -> m [Int]
toList (IntListM fptr) = liftIO $ withForeignPtr fptr $ toList' where
    toList' ptr = do
        peek ptr >>= \case
          Null -> return []
          Cons i tailPtr -> (i:) <$> toList' tailPtr
{-# INLINE toList #-}


-- === Memory management === --

listFinalizer :: MonadIO m => Ptr IntListMData -> m ()
listFinalizer = liftIO . freeAll where
    freeAll lstPtr = do
        peek lstPtr >>= \case
          Null -> return ()
          Cons _ tailPtr -> freeAll tailPtr
        Ptr.free lstPtr
{-# INLINE listFinalizer #-}


-- === Instances === --


instance Show IntListM where
    show = show . unsafePerformIO . toList ; {-# NOINLINE show #-}

chunkSize :: Int
chunkSize = sizeOf' @Int ; {-# INLINE chunkSize #-}

instance Storable IntListMData where
    sizeOf    _ = 2 * chunkSize ; {-# INLINE sizeOf    #-}
    alignment _ = chunkSize     ; {-# INLINE alignment #-}
    peek ptr = peek (castPtr ptr) >>= \tailPtr -> if tailPtr == nullPtr
        then return Null
        else Cons <$> peekByteOff ptr chunkSize <*> pure tailPtr
    {-# INLINE peek #-}
    poke ptr = \case
        Null            -> poke (castPtr ptr) nullPtr
        Cons el tailPtr -> poke (castPtr ptr) tailPtr
                        >> pokeByteOff ptr chunkSize el
    {-# INLINE poke #-}
