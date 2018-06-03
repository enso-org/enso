module Data.AutoVector.Mutable.Storable
    (module Data.AutoVector.Mutable.Storable, module X) where
import Data.AutoVector.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, fromList, length, toList,
                 unsafeRead)

import qualified Data.Construction         as Data
import qualified Data.List                 as List
import qualified Foreign.DynamicStorable   as DynamicStorable
import qualified Foreign.Marshal.Alloc     as Mem
import qualified Foreign.Marshal.Utils     as Mem
import qualified Foreign.Storable.Deriving as Storable
import qualified Foreign.Storable.Utils    as Storable

import Foreign.DynamicStorable (DynamicStorable)
import Foreign.Ptr             (Ptr, nullPtr, plusPtr)
import Foreign.Storable        (Storable)
import Foreign.Storable.Utils  (castPeekAndOffset, castPokeAndOffset)
import System.IO.Unsafe        (unsafeDupablePerformIO, unsafePerformIO)







--------------------
-- === Array === --
--------------------

-- === Definition === --

data Array a = Array
    { __length :: !Int
    , __size   :: !Int
    , __ptr    :: !(Ptr a)
    }
makeLenses      ''Array
Storable.derive ''Array

type instance Item (Array a) = a


-- === Array instances === --

instance StaticLength (Array a) where
    staticLength = view array_length
    {-# INLINE staticLength #-}

instance StaticSize (Array a) where
    staticSize = view array_size
    {-# INLINE staticSize #-}

instance Empty (Array a) where
    empty = Array 0 0 nullPtr
    {-# INLINE empty #-}

instance (MonadIO m, Storable a)
      => New m (Array a) where
    new = \size -> do
        let byteSize = size * Storable.sizeOf' @a
        ptr <- liftIO $ Mem.mallocBytes byteSize
        pure $ Array 0 size ptr
    {-# INLINE new #-}

instance MonadIO m
      => Free m (Array a) where
    free = liftIO . Mem.free . view array_ptr
    {-# INLINE free #-}

instance (MonadIO m, Storable a)
      => Read m (Array a) where
    unsafeRead = \a ix -> liftIO $ Storable.peekElemOff (a ^. array_ptr) ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Storable a)
      => Write m (Array a) where
    unsafeWrite = \a ix val
        -> liftIO $ Storable.pokeElemOff (a ^. array_ptr) ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Storable a)
      => FromList m (Array a) where
    fromList = \lst -> liftIO $ do
        a <- new $! List.length lst
        mapM_ (uncurry $ unsafeWrite a) $ zip [0..] lst
        pure a
    {-# INLINE fromList #-}

instance (MonadIO m, Storable a)
      => ToList m (Array a) where
    toList = \a -> mapM (unsafeRead a) [0 .. staticLength a - 1]
    {-# INLINE toList #-}

instance (MonadIO m, Storable a)
      => StaticGrow m (Array a) where
    staticGrow = \a -> liftIO $ do
        let size'         = staticSize a * 2
            len           = staticLength a
            ptr           = a ^. array_ptr
            elemByteSize  = Storable.sizeOf' @a
            bytesToMalloc = elemByteSize * size'
            bytesToCopy   = elemByteSize * len
        ptr' <- Mem.mallocBytes bytesToMalloc
        Mem.copyBytes ptr' ptr bytesToCopy
        pure $ Array len size' ptr'
    {-# INLINE staticGrow #-}

unsafeArrayPushBack :: (MonadIO m, Storable a) => Array a -> a -> m (Array a)
unsafeArrayPushBack = \a v -> do
    unsafeWrite a (staticLength a) v
    let a' = a & array_length %~ (+1)
    pure a'
{-# INLINE unsafeArrayPushBack #-}



-- === Debug === --

instance (Show a, Storable a) => Show (Array a) where
    show = show . unsafePerformIO . toList ; {-# NOINLINE show #-}

instance Mempty (Array a) where mempty = empty ; {-# INLINE mempty #-}


-- === Instances === --

instance Eq (Array a) where
    (==) = (==) `on` view array_ptr ; {-# INLINE (==) #-}




--------------------
-- === Vector === --
--------------------

-- === Definition === --

newtype Vector a = Vector (Ptr (Array a))
makeLenses ''Vector

type instance Item (Vector a) = a

-- === Utils === --

-- !!! UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE
-- !!! UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE
-- !!! UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE UNSAFE
-- Error prone implementation. Hardcoding offset in Array
-- vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

readArrayLength :: MonadIO m => Vector a -> m Int
readArrayLength = \a -> liftIO $ Storable.peek (coerce $ unwrap a)
{-# INLINE readArrayLength #-}

readArraySize :: MonadIO m => Vector a -> m Int
readArraySize = \a
    -> liftIO $ Storable.peekByteOff (unwrap a) (Storable.sizeOf' @Int)
{-# INLINE readArraySize #-}

readArrayMem :: MonadIO m => Vector a -> m (Ptr a)
readArrayMem = \a
    -> liftIO $ Storable.peekByteOff (unwrap a) (2 * Storable.sizeOf' @Int)
{-# INLINE readArrayMem #-}

-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


-- === Instances === --

instance MonadIO m => Length m (Vector a) where
    length = readArrayLength
    {-# INLINE length #-}

instance MonadIO m => Size m (Vector a) where
    size = readArraySize
    {-# INLINE size #-}

instance (MonadIO m, Storable a)
      => New m (Vector a) where
    new = \size -> liftIO $ do
        arr <- new size
        wrap <$> Mem.new arr
    {-# INLINE new #-}

instance (MonadIO m, Storable a)
      => Free m (Vector a) where
    free = \a -> liftIO $ do
        mem <- readArrayMem a
        Mem.free mem
        Mem.free (unwrap a)
    {-# INLINE free #-}

instance (MonadIO m, Storable a)
      => Read m (Vector a) where
    unsafeRead = \a ix -> liftIO $ do
        mem <- readArrayMem a
        Storable.peekElemOff mem ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Storable a)
      => Write m (Vector a) where
    unsafeWrite = \a ix val -> liftIO $ do
        mem <- readArrayMem a
        Storable.pokeElemOff mem ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Storable a)
      => Grow m (Vector a) where
    grow = \a -> liftIO $ do
        arr  <- Storable.peek $ unwrap a
        arr' <- staticGrow arr
        free arr
        Storable.poke (unwrap a) arr'
    {-# INLINE grow #-}

instance (MonadIO m, Storable a)
      => PushBack m (Vector a) where
    pushBack = \a v -> liftIO $ do
        arr   <- Storable.peek $ unwrap a
        arr'  <- if staticLength arr == staticSize arr
                 then staticGrow arr else pure arr
        arr'' <- unsafeArrayPushBack arr' v
        Storable.poke (unwrap a) arr''
    {-# INLINE pushBack #-}
