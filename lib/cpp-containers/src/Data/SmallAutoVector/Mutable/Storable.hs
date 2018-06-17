{-# LANGUAGE UndecidableInstances #-}

module Data.SmallAutoVector.Mutable.Storable
    (module Data.SmallAutoVector.Mutable.Storable, module X) where
import Data.AutoVector.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, length, toList,
                 unsafeRead, w)

import qualified Data.AutoVector.Mutable.Storable as Vector
import qualified Data.Construction                as Data
import qualified Data.List                        as List
import qualified Data.Storable                    as Struct
import qualified Foreign.Marshal.Alloc            as Mem
import qualified Foreign.Marshal.Utils            as Mem
import qualified Foreign.Storable.Class           as Storable
import qualified Type.Known                       as Type

import Data.AutoVector.Mutable.Storable (Vector)
import Data.Storable                    (type (-::), Struct)
import Foreign.Ptr                      (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable                 (Storable)
import Foreign.Storable.Class           (Copy, View)
import Foreign.Storable.Utils           (castPeekAndOffset, castPokeAndOffset)
import System.IO.Unsafe                 (unsafeDupablePerformIO,
                                         unsafePerformIO)



----------------------
-- === MemChunk === --
----------------------

-- === Definition === --

newtype MemChunk (n :: Nat) (a :: Type) = MemChunk (Ptr a)
makeLenses ''MemChunk


-- === API === --

unsafeNullMemChunk :: MemChunk n a
unsafeNullMemChunk = wrap nullPtr
{-# INLINE unsafeNullMemChunk #-}


-- === Instances === --

instance (Storable.KnownStaticSize t a, Type.KnownInt n)
      => Storable.KnownStaticSize t (MemChunk n a) where
    staticSize = Type.val' @n * Storable.staticSize @t @a
    {-# INLINE staticSize #-}

instance Applicative m
      => Storable.Peek t m (MemChunk n a) where
    peek = pure . coerce
    {-# INLINE peek #-}



-------------------------
-- === SmallVector === --
-------------------------

-- === Definition === --

newtype SmallVector (n :: Nat) a = SmallVector (Struct (Layout n a))
type Layout n a =
   '[ "length"   -:: Int
    , "size"     -:: Int
    , "offset"   -:: Int
    , "localMem" -:: MemChunk n a
    ]
makeLenses ''SmallVector

type instance Item (SmallVector n a) = a
instance Struct.IsStruct (SmallVector n a)


-- === Fields === --

_length   :: Struct.FieldRef "length"
_size     :: Struct.FieldRef "size"
_offset   :: Struct.FieldRef "offset"
_localMem :: Struct.FieldRef "localMem"
_length   = Struct.field ; {-# INLINE _length   #-}
_size     = Struct.field ; {-# INLINE _size     #-}
_offset   = Struct.field ; {-# INLINE _offset   #-}
_localMem = Struct.field ; {-# INLINE _localMem #-}


-- === Utils === --

placementNew :: ∀ n a m.
    (MonadIO m, Type.KnownInt n)
    => Ptr (SmallVector n a) -> m (SmallVector n a)
placementNew = \ptr -> liftIO $ do
    let vec = Struct.unsafeCastFromPtr ptr
    Struct.writeField _length vec 0
    Struct.writeField _size   vec $! Type.val' @n
    Struct.writeField _offset vec 0
    pure vec
{-# INLINE placementNew #-}

elemsPtr :: MonadIO m => SmallVector n a -> m (Ptr a)
elemsPtr = \a -> do
    offset <- Struct.readField _offset a
    let localMemPtr = Struct.fieldPtr _localMem a
    pure $ localMemPtr `plusPtr` offset
{-# INLINE elemsPtr #-}


-- === Instances === --

instance (Type.KnownInt n, Storable.KnownStaticSize t a)
      => Storable.KnownStaticSize t (SmallVector n a) where
    staticSize = Storable.staticSize @t @Int                -- length
               + Storable.staticSize @t @Int                -- size
               + Storable.staticSize @t @Int                -- ptrOff
               + (Storable.staticSize @t @a * Type.val' @n) -- elems
    {-# INLINE staticSize #-}

instance
    ( MonadIO m
    , Storable.KnownStaticSize Copy (SmallVector n a)
    , Type.KnownInt n
    ) => New m (SmallVector n a) where
    new = do
        ptr <- liftIO . Mem.mallocBytes
             $ Storable.staticSize @Copy @(SmallVector n a)
        placementNew ptr
    {-# INLINE new #-}

instance MonadIO m
      => Length m (SmallVector n a) where
    length = Struct.readField _length
    {-# INLINE length #-}

instance MonadIO m
      => Size m (SmallVector n a) where
    size = Struct.readField _size
    {-# INLINE size #-}

instance MonadIO m
      => Free m (SmallVector n a) where
    free = \a -> liftIO $ do
        offset <- Struct.readField _offset a
        when (offset /= 0) $ do
            let localMemPtr = Struct.fieldPtr _localMem a
            Mem.free $ localMemPtr `plusPtr` offset
        Struct.free a
    {-# INLINE free #-}

instance (MonadIO m, Storable.StaticPeek View m a)
      => Read m (SmallVector n a) where
    unsafeRead = \a ix -> do
        ptr <- elemsPtr a
        Storable.peekElemOff @View ptr ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Storable.StaticPoke View m a)
      => Write m (SmallVector n a) where
    unsafeWrite = \a ix val -> do
        ptr <- elemsPtr a
        Storable.pokeElemOff @View ptr ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Storable.StaticPeek View m a)
      => ToList m (SmallVector n a) where
    toList = \a -> do
        len <- length a
        mapM (unsafeRead a) [0 .. len - 1]
    {-# INLINE toList #-}

instance (MonadIO m, Storable.KnownStaticSize View a)
      => Grow m (SmallVector n a) where
    grow = \a -> do
        oldSize   <- size     a
        elemCount <- length   a
        ptr       <- elemsPtr a
        offset    <- Struct.readField _offset a
        let newSize       = oldSize * 2
            elemByteSize  = Storable.staticSize @View @a
            bytesToMalloc = elemByteSize * newSize
            bytesToCopy   = elemByteSize * elemCount
            localMemPtr   = Struct.fieldPtr _localMem a
        newElemsPtr <- liftIO $ Mem.mallocBytes bytesToMalloc
        liftIO $ Mem.copyBytes newElemsPtr ptr bytesToCopy
        when (offset /= 0) $
            liftIO (Mem.free ptr)
        Struct.writeField _size   a newSize
        Struct.writeField _offset a $! newElemsPtr `minusPtr` localMemPtr
    {-# INLINE grow #-}

instance (MonadIO m, Storable.StaticPoke View m a)
      => PushBack m (SmallVector n a) where
    pushBack = \a v -> do
        len <- length a
        siz <- size   a
        when (len == siz) $ grow a
        unsafeWrite a len v
        Struct.writeField _length a $! len + 1
    {-# INLINE pushBack #-}
