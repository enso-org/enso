{-# LANGUAGE UndecidableInstances #-}

module Data.SmallAutoVector.Mutable.Storable
    (module Data.SmallAutoVector.Mutable.Storable, module X) where
import Data.AutoVector.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, fromList, toList,
                 unsafeRead, length)

import qualified Data.AutoVector.Mutable.Storable as Vector
import qualified Data.Construction                as Data
import qualified Data.List                        as List
import qualified Data.Storable                    as Struct
import qualified Foreign.Marshal.Alloc            as Mem
import qualified Foreign.Marshal.Utils            as Mem
import qualified Foreign.Storable.Deriving        as Storable
import qualified Foreign.Storable.Utils           as Storable
import qualified Type.Known                       as Type

import Data.AutoVector.Mutable.Storable (Vector)
import Data.Storable                    (type (-::), Struct)
import Foreign.Ptr                      (Ptr, nullPtr, plusPtr)
import Foreign.Storable                 (Storable)
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

type instance Struct.ByteSize t (MemChunk n a) = n * Struct.ByteSize t a



-------------------------
-- === SmallVector === --
-------------------------

-- === Definition === --

newtype SmallVector (n :: Nat) a = SmallVector (Struct (Layout n a))
type Layout n a =
   '[ "length"     -:: Int
    , "size"       -:: Int
    , "elemsPtr"   -:: (Ptr a)
    , "smallChunk" -:: MemChunk n a
    ]
makeLenses ''SmallVector

type instance Item (SmallVector n a) = a
instance Struct.IsStruct (SmallVector n a)


-- === Fields === --

_length     :: Struct.FieldRef "length"
_size       :: Struct.FieldRef "size"
_elemsPtr   :: Struct.FieldRef "elemsPtr"
_smallChunk :: Struct.FieldRef "smallChunk"
_length     = Struct.field ; {-# INLINE _length     #-}
_size       = Struct.field ; {-# INLINE _size       #-}
_elemsPtr   = Struct.field ; {-# INLINE _elemsPtr   #-}
_smallChunk = Struct.field ; {-# INLINE _smallChunk #-}


-- === Utils === --

-- | WARNING
--   Do not use 'free' when using this construction utility because the base
--   memory will not be allocated here and it should not be freed by this 
--   implementation.
unsafeNewInMemory :: ∀ n a m. 
    (MonadIO m, Type.KnownInt n, Struct.Constructor (SmallVector n a))
    => Ptr () -> m (SmallVector n a)
unsafeNewInMemory = \ptr -> liftIO $ do
    let vec = Struct.unsafeCastFromPtr ptr
    Struct.writeField _length   vec 0
    Struct.writeField _size     vec $! Type.val' @n
    Struct.writeField _elemsPtr vec $! smallChunkElemPtr vec
    pure vec
{-# INLINE unsafeNewInMemory #-}

smallChunkElemPtr :: SmallVector n a -> Ptr a
smallChunkElemPtr = coerce . Struct.fieldPtr _smallChunk
{-# INLINE smallChunkElemPtr #-}


-- === Instances === --

instance MonadIO m => Length m (SmallVector n a) where
    length = Struct.readField _length
    {-# INLINE length #-}

instance MonadIO m => Size m (SmallVector n a) where
    size = Struct.readField _size
    {-# INLINE size #-}

instance MonadIO m
      => Free m (SmallVector n a) where
    free = \a -> liftIO $ do
        elemsPtr <- Struct.readField _elemsPtr a
        when (elemsPtr /= smallChunkElemPtr a)
            Mem.free elemsPtr
        Struct.free a
    {-# INLINE free #-}

instance (MonadIO m, Storable a)
      => Read m (SmallVector n a) where
    unsafeRead = \a ix -> liftIO $ do
        elemsPtr <- Struct.readField _elemsPtr a
        Storable.peekElemOff elemsPtr ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Storable a)
      => Write m (SmallVector n a) where
    unsafeWrite = \a ix val -> liftIO $ do
        elemsPtr <- Struct.readField _elemsPtr a
        Storable.pokeElemOff elemsPtr ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Storable a)
      => ToList m (SmallVector n a) where
    toList = \a -> do
        len <- length a
        mapM (unsafeRead a) [0 .. len - 1]
    {-# INLINE toList #-}

instance (MonadIO m, Storable a)
      => Grow m (SmallVector n a) where
    grow = \a -> liftIO $ do
        oldSize   <- size   a
        elemCount <- length a
        elemsPtr  <- Struct.readField _elemsPtr a
        let newSize       = oldSize * 2
            elemByteSize  = Storable.sizeOf' @a
            bytesToMalloc = elemByteSize * newSize
            bytesToCopy   = elemByteSize * elemCount
        newElemsPtr <- Mem.mallocBytes bytesToMalloc
        Mem.copyBytes newElemsPtr elemsPtr bytesToCopy
        when (elemsPtr /= smallChunkElemPtr a)
            Mem.free elemsPtr
        Struct.writeField _size     a newSize
        Struct.writeField _elemsPtr a newElemsPtr
    {-# INLINE grow #-}

instance (MonadIO m, Storable a)
      => PushBack m (SmallVector n a) where
    pushBack = \a v -> liftIO $ do
        len <- length a
        siz <- size   a
        when (len == siz) $ grow a
        unsafeWrite a len v
        Struct.writeField _length a (len + 1)
    {-# INLINE pushBack #-}
