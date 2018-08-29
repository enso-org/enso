{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.AutoVector.Mutable.Storable
    (module Data.AutoVector.Mutable.Storable, module X) where
import Data.Mutable.Class as X

-- import Prologue hiding (FromList, Read, ToList, empty, fromList, length, toList,
--                  unsafeRead)

-- import qualified Data.Construction         as Data
-- import qualified Data.List                 as List
-- import qualified Data.Storable             as Struct
-- import qualified Foreign.Marshal.Alloc     as Mem
-- import qualified Foreign.Marshal.Utils     as Mem
-- import qualified Foreign.Storable.Deriving as Storable
-- import qualified Foreign.Storable.Utils    as Storable
-- import qualified Type.Known                as Type

-- import Data.Storable          (type (-::), Struct)
-- import Foreign.Ptr            (Ptr, nullPtr, plusPtr)
-- import Foreign.Storable       (Storable)
-- import Foreign.Storable.Utils (castPeekAndOffset, castPokeAndOffset)
-- import Unsafe.Coerce


-- --------------------
-- -- === Vector === --
-- --------------------

-- -- === Definition === --

-- newtype Vector       a = Vector (Struct (VectorLayout a)) deriving (NFData)
-- type    VectorLayout a =
--    '[ "length"   -:: Int
--     , "size"     -:: Int
--     , "elemsPtr" -:: (Ptr a)
--     ]

-- makeLenses ''Vector

-- type instance Item (Vector a) = a
-- instance Struct.IsStruct (Vector a)


-- -- === Fields === --

-- _length   :: Struct.FieldRef "length"
-- _size     :: Struct.FieldRef "size"
-- _elemsPtr :: Struct.FieldRef "elemsPtr"
-- _length   = Struct.field ; {-# INLINE _length   #-}
-- _size     = Struct.field ; {-# INLINE _size     #-}
-- _elemsPtr = Struct.field ; {-# INLINE _elemsPtr #-}


-- -- === Utils === --

-- unsafeCastFromPtr :: Ptr t -> Vector a
-- unsafeCastFromPtr = Struct.unsafeCastFromPtr
-- {-# INLINE unsafeCastFromPtr #-}


-- -- === Instances === --

-- instance MonadIO m => Length m (Vector a) where
--     length = Struct.readField _length
--     {-# INLINE length #-}

-- instance MonadIO m => Size m (Vector a) where
--     size = Struct.readField _size
--     {-# INLINE size #-}

-- instance (MonadIO m, Storable a)
--       => Alloc m (Vector a) where
--     alloc = \size -> liftIO $ do
--         let elemsByteSize = size * Storable.sizeOf' @a
--         elemsPtr <- liftIO $ Mem.mallocBytes elemsByteSize
--         Struct.construct @(Vector a) 0 size elemsPtr
--     {-# INLINE alloc #-}

-- instance MonadIO m
--       => Free m (Vector a) where
--     free = \a -> liftIO $ do
--         elemsPtr <- Struct.readField _elemsPtr a
--         Mem.free elemsPtr
--         Struct.free a
--     {-# INLINE free #-}

-- instance (MonadIO m, Storable a)
--       => Read m (Vector a) where
--     unsafeRead = \a ix -> liftIO $ do
--         elemsPtr <- Struct.readField _elemsPtr a
--         Storable.peekElemOff elemsPtr ix
--     {-# INLINE unsafeRead #-}

-- instance (MonadIO m, Storable a)
--       => Write m (Vector a) where
--     unsafeWrite = \a ix val -> liftIO $ do
--         elemsPtr <- Struct.readField _elemsPtr a
--         Storable.pokeElemOff elemsPtr ix val
--     {-# INLINE unsafeWrite #-}

-- instance (MonadIO m, Storable a)
--       => FromList m (Vector a) where
--     fromList = \lst -> liftIO $ do
--         a <- alloc $! List.length lst
--         mapM_ (pushBack a) lst
--         pure a
--     {-# INLINE fromList #-}

-- instance (MonadIO m, Storable a)
--       => ToList m (Vector a) where
--     toList = \a -> do
--         len <- length a
--         mapM (unsafeRead a) [0 .. len - 1]
--     {-# INLINE toList #-}

-- instance (MonadIO m, Storable a)
--       => Grow m (Vector a) where
--     grow = \a -> liftIO $ do
--         oldSize   <- size   a
--         elemCount <- length a
--         elemsPtr  <- Struct.readField _elemsPtr a
--         let newSize       = oldSize * 2
--             elemByteSize  = Storable.sizeOf' @a
--             bytesToMalloc = elemByteSize * newSize
--             bytesToCopy   = elemByteSize * elemCount
--         newElemsPtr <- Mem.mallocBytes bytesToMalloc
--         Mem.copyBytes newElemsPtr elemsPtr bytesToCopy
--         Mem.free elemsPtr
--         Struct.writeField _size     a newSize
--         Struct.writeField _elemsPtr a newElemsPtr
--     {-# INLINE grow #-}

-- instance (MonadIO m, Storable a)
--       => PushBack m (Vector a) where
--     pushBack = \a v -> liftIO $ do
--         len <- length a
--         siz <- size   a
--         when (len == siz) $ grow a
--         unsafeWrite a len v
--         Struct.writeField _length a $! len + 1
--     {-# INLINE pushBack #-}

-- insert :: ∀ a m. (MonadIO m, Storable a) => Vector a -> Int -> a -> m ()
-- insert = \v ix a -> liftIO $ do
--     len  <- length v
--     siz  <- size   v
--     when (len == siz) $ grow v
--     ptr0 <- Struct.readField _elemsPtr v
--     let elSize  = Storable.sizeOf' @a
--         ptrIx   = ptr0  `plusPtr` (elSize * ix)
--         ptrIx'  = ptrIx `plusPtr` elSize
--         byteOff = elSize * (len - ix)
--     when (byteOff > 0) $ Mem.moveBytes ptrIx' ptrIx byteOff
--     unsafeWrite v ix a
--     Struct.writeField _length v $! len + 1
-- {-# INLINE insert #-}

-- remove :: ∀ a m. (MonadIO m, Storable a) => Vector a -> Int -> m ()
-- remove = \v ix -> liftIO $ do
--     len  <- length v
--     ptr0 <- Struct.readField _elemsPtr v
--     let elSize  = Storable.sizeOf' @a
--         ptrIx   = ptr0  `plusPtr` (elSize * ix)
--         ptrIx'  = ptrIx `plusPtr` elSize
--         byteOff = elSize * (len - ix - 1)
--     when (byteOff > 0) $ Mem.moveBytes ptrIx ptrIx' byteOff
--     Struct.writeField _length v $! len - 1
-- {-# INLINE remove #-}


-- -- === Debug === --

