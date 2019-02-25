{-# OPTIONS_GHC -Wno-missing-methods #-}

{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Storable.Array
    (module Data.Mutable.Storable.Array, module X) where

import Data.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, length, toList,
                 unsafeRead)

-- import qualified Data.AutoVector.Mutable.Storable as Vector
-- import qualified Data.Construction                as Data
-- import qualified Data.Convert2.Class              as Convert
-- import qualified Data.List                        as List
-- import qualified Data.Property                    as Property
-- import qualified Data.Storable                    as Struct
-- import qualified Foreign.Marshal.Alloc            as Mem
-- import qualified Foreign.Marshal.Utils            as Mem
-- import qualified Foreign.Storable                 as StdStorable
import qualified Foreign.Storable.Class           as Storable
import qualified Memory                           as Memory
import qualified Type.Known                       as Type

-- import Data.Storable          (type (-::), UnmanagedStruct)
-- import Foreign.Ptr            (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable.Class (View)
-- import Foreign.Storable.Utils (Dynamic, Dynamics)
-- import Foreign.Storable.Utils (castPeekAndOffset, castPokeAndOffset)



-- ----------------------
-- -- === MemChunk === --
-- ----------------------

-- -- === Definition === --

-- -- TODO: remove, replace with Memory.ConstantRegion
-- newtype MemChunk (n :: Nat) (a :: Type) = MemChunk (Ptr a)
-- makeLenses ''MemChunk


-- -- === API === --

-- unsafeNullMemChunk :: MemChunk n a
-- unsafeNullMemChunk = wrap nullPtr
-- {-# INLINE unsafeNullMemChunk #-}


-- -- === Instances === --

-- instance (Storable.KnownConstantSize a, Type.KnownInt n)
--       => Storable.KnownConstantSize (MemChunk n a) where
--     constantSize = Type.val' @n * Storable.constantSize @a
--     {-# INLINE constantSize #-}

-- instance Applicative m
--       => Storable.Peek Struct.Field m (MemChunk n a) where
--     peek = pure . coerce
--     {-# INLINE peek #-}



-------------------------
-- === Array === --
-------------------------

-- === Definition === --

type    Array__ t (n :: Nat) a = Memory.Ptr t a
newtype Array   t (n :: Nat) a = Array (Array__ t n a)
    deriving (Generic)
makeLenses ''Array


-- === Aliases === --

type UnmanagedArray = Array 'Memory.Unmanaged
type ManagedArray   = Array 'Memory.Managed


-- === Instances === --

type instance Item (Array t n a) = a
type instance Memory.Management (Array t n a) = t

instance Eq     (Array__ t n a) => Eq     (Array t n a)
instance NFData (Array__ t n a) => NFData (Array t n a)
instance Ord    (Array__ t n a) => Ord    (Array t n a)


-- === API Instances === --

instance (Storable.KnownConstantSize a, Type.KnownInt n)
      => Storable.KnownConstantSize (Array t n a) where
    constantSize = Type.val' @n * Storable.constantSize @a
    {-# INLINE constantSize #-}

-- instance ()
--       => PlacementNew m (Array t n a) where
--     placementNew = const $ pure ()
--     {-# INLINE placementNew #-}

-- instance
--     ( MonadIO m
--     , Storable.KnownConstantSize (Array t n a)
--     , Type.KnownInt n
--     ) => New m (Array t n a) where
--     new = do
--         ptr <- liftIO . Mem.mallocBytes
--              $ Storable.constantSize @(Array t n a)
--         placementNew ptr
--     {-# INLINE new #-}

instance (Type.KnownInt n, Applicative m)
      => Size m (Array t n a) where
    size = const . pure $ Type.val' @n
    {-# INLINE size #-}

instance (Type.KnownInt n, Applicative m)
      => Capacity m (Array t n a) where
    capacity = const . pure $ Type.val' @n
    {-# INLINE capacity #-}

-- instance MonadIO m
--       => Free m (Array t n a) where
--     free = \a -> liftIO $ do
--         whenM (usesDynamicMemory a)
--             $ Mem.free =<< Struct.readField _externalMem a
--         Struct.free a
--     {-# INLINE free #-}

-- instance (MonadIO m, Storable.StaticPeek View m a)
--       => Read m (Array t n a) where
--     unsafeRead = \a ix -> do
--         ptr <- elemsPtr a
--         Storable.peekElemOff @View ptr ix
--     {-# INLINE unsafeRead #-}

instance
    ( MonadIO m
    , Storable.Poke View m a
    , Storable.KnownConstantSize a
    , Memory.PtrType t
    ) => Write m (Array t n a) where
    unsafeWrite = \a ix val -> Memory.withUnmanagedPtr (unwrap a)
                             $ \p -> Storable.pokeElemOff @View p ix val
    {-# INLINE unsafeWrite #-}

instance
    ( MonadIO m
    , Storable.Peek View m a
    , Storable.KnownConstantSize a
    , Memory.PtrType t
    ) => Read m (Array t n a) where
    unsafeRead = \a ix -> Memory.withUnmanagedPtr (unwrap a)
                        $ \p -> Storable.peekElemOff @View p ix
    {-# INLINE unsafeRead #-}

-- instance (MonadIO m, Storable.StaticPeek View m a)
--       => ToList m (Array t n a) where
--     toList = \a -> do
--         len <- size a
--         mapM (unsafeRead a) [0 .. len - 1]
--     {-# INLINE toList #-}

-- instance (Monad m, New m (Array t n a), PushBack m (Array t n a))
--       => FromList m (Array t n a) where
--     fromList = \lst -> do
--         a <- new
--         mapM_ (pushBack a) lst
--         pure a
--     {-# INLINE fromList #-}

-- instance (MonadIO m, Storable.KnownConstantSize a)
--       => Grow m (Array t n a) where
--     grow = \a -> do
--         oldCapacity <- capacity a
--         elemCount   <- size     a
--         ptr         <- elemsPtr a
--         let newSize       = if oldCapacity == 0 then 16 else oldCapacity * 2
--             elemByteSize  = Storable.constantSize @a
--             bytesToMalloc = elemByteSize * newSize
--             bytesToCopy   = elemByteSize * elemCount
--         newElemsPtr <- liftIO $ Mem.mallocBytes bytesToMalloc
--         liftIO $ Mem.copyBytes newElemsPtr ptr bytesToCopy
--         whenM (usesDynamicMemory a) $
--             liftIO (Mem.free ptr)
--         Struct.writeField _capacity a newSize
--         Struct.writeField _externalMem a $! newElemsPtr
--     {-# INLINE grow #-}

-- instance (MonadIO m, Storable.StaticPoke View m a)
--       => PushBack m (Array t n a) where
--     pushBack = \a v -> do
--         siz <- size     a
--         cap <- capacity a
--         when (siz == cap) $ grow a
--         unsafeWrite a siz v
--         Struct.writeField _length a $! siz + 1
--     {-# INLINE pushBack #-}

-- instance
--     ( MonadIO m
--     , Grow  m (Array t n a)
--     , Write m (Array t n a)
--     , Storable.KnownConstantSize a
--     ) => InsertAt m (Array t n a) where
--     insertAt = \a ix v -> do
--         siz <- size     a
--         cap <- capacity a
--         when (siz == cap) $ grow a
--         ptr0 <- elemsPtr a
--         let elSize  = Storable.constantSize @a
--             ptrIx   = ptr0  `plusPtr` (elSize * ix)
--             ptrIx'  = ptrIx `plusPtr` elSize
--             byteOff = elSize * (siz - ix)
--         when (byteOff > 0) $ liftIO $ Mem.moveBytes ptrIx' ptrIx byteOff
--         unsafeWrite a ix v
--         Struct.writeField _length a $! siz + 1
--     {-# INLINE insertAt #-}

-- instance
--     ( MonadIO m
--     , Storable.KnownConstantSize a
--     ) => RemoveAt m (Array t n a) where
--     removeAt = \a ix -> do
--         siz  <- size a
--         ptr0 <- elemsPtr a
--         let elSize  = Storable.constantSize @a
--             ptrIx   = ptr0  `plusPtr` (elSize * ix)
--             ptrIx'  = ptrIx `plusPtr` elSize
--             byteOff = elSize * (siz - ix - 1)
--         when (byteOff > 0) $ liftIO $ Mem.moveBytes ptrIx ptrIx' byteOff
--         Struct.writeField _length a $! siz - 1
--     {-# INLINE removeAt #-}



-- -- === Memory management instances === --

-- instance Applicative m
--       => Storable.Peek View m (Array t n a) where
--     peek = pure . coerce
--     {-# INLINE peek #-}

-- instance (MonadIO m, Storable.KnownConstantSize (Array t n a), Show (Array t n a))
--       => Storable.Poke View m (Array t n a) where
--     poke = \ptr a ->
--         let size = Storable.constantSize @(Array t n a)
--         in  liftIO $ Mem.copyBytes ptr (coerce a) size

--     {-# INLINE poke #-}

-- instance MonadIO m => Data.ShallowDestructor1 m (Array n) where
--     destructShallow1 = free
--     {-# INLINE destructShallow1 #-}


-- -- === Debug instances === --




-- -- === Deprecated instances === --

-- type instance Property.Get Dynamics (Array t n a) = Dynamic

-- instance
--     ( Storable.Peek View IO a
--     , Storable.KnownConstantSize (Array t n a)
--     , Storable View IO (Array t n a)
--     , Type.KnownInt n
--     ) => StdStorable.Storable (Array t n a) where
--     sizeOf    = \ ~_ -> Storable.constantSize @(Array t n a)
--     alignment = \ ~_ -> StdStorable.alignment (undefined :: Int)
--     peek      = Storable.peek @View
--     poke      = Storable.poke @View
--     {-# INLINE sizeOf #-}


-- -- WARNING: this instance is strange. It does not release self-memory,
-- --          because it is used for placement-new objects
-- instance MonadIO m
--       => Data.Destructor1 m (Array n) where
--     destruct1 = \a -> liftIO $ do
--         whenM (usesDynamicMemory a)
--             $ Mem.free =<< Struct.readField _externalMem a
--     {-# INLINE destruct1 #-}

