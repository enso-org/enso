{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}

module Memory.Data.Ptr where

import Prologue

import qualified Data.Convert2.Class       as Convert
import qualified Foreign.ForeignPtr        as Raw
import qualified Foreign.ForeignPtr.Unsafe as Raw
import qualified Foreign.Marshal.Alloc     as Raw
import qualified Foreign.Marshal.Utils     as Raw
import qualified Foreign.Ptr               as Raw
import qualified GHC.ForeignPtr            as Raw
import qualified Memory.Management         as Memory

import GHC.Base         (Int (I#), minusAddr#)
import System.IO.Unsafe (unsafePerformIO)


-----------------
-- === Ptr === --
-----------------

-- === Definition === -

newtype Ptr (t :: Memory.ManagementType) a = Ptr (PtrImpl t a)

type family PtrImpl (t :: Memory.ManagementType) :: Type -> Type where
    PtrImpl 'Memory.Managed   = Raw.ForeignPtr
    PtrImpl 'Memory.Unmanaged = Raw.Ptr


-- === Aliases === --

type SomePtr t        = Ptr t ()
type ManagedPtr       = Ptr 'Memory.Managed
type UnmanagedPtr     = Ptr 'Memory.Unmanaged
type SomeManagedPtr   = SomePtr 'Memory.Managed
type SomeUnmanagedPtr = SomePtr 'Memory.Unmanaged


-- === Conversions === --

instance (a ~ b, t ~ 'Memory.Unmanaged)
      => Convert.To (Ptr t a) (Raw.Ptr b) where
    to = wrap ; {-# INLINE to #-}

instance (a ~ b, t ~ 'Memory.Managed)
      => Convert.To (Ptr t a) (Raw.ForeignPtr b) where
    to = wrap ; {-# INLINE to #-}

coercePtr :: Ptr t a -> Ptr t b
coercePtr = unsafeCoerce
{-# INLINE coercePtr #-}

toRawPtr :: Ptr t a -> PtrImpl t a
toRawPtr = unwrap
{-# INLINE toRawPtr #-}


-- === Arithmetics === --

class PtrType t where
    plus              :: ∀ src tgt. Ptr t src -> Int -> Ptr t tgt
    minus             :: ∀ src tgt. Ptr t tgt -> Ptr t src -> Int
    nullPtr           :: ∀ a. Ptr t a
    unsafeToUnmanaged :: ∀ a. Ptr t a -> UnmanagedPtr a
    withUnmanagedPtr  :: ∀ a b m. MonadIO m => Ptr t a -> (UnmanagedPtr a -> m b) -> m b
    mallocBytesIO     :: ∀ a. Int -> IO (Ptr t a)


instance PtrType 'Memory.Unmanaged where
    plus              = \ptr -> wrap . Raw.plusPtr (unwrap ptr)
    minus             = \tgt src -> Raw.minusPtr (unwrap tgt) (unwrap src)
    nullPtr           = wrap Raw.nullPtr
    withUnmanagedPtr = flip ($)
    unsafeToUnmanaged = id
    mallocBytesIO     = fmap wrap . Raw.mallocBytes
    {-# INLINE plus              #-}
    {-# INLINE minus             #-}
    {-# INLINE nullPtr           #-}
    {-# INLINE unsafeToUnmanaged #-}
    {-# INLINE withUnmanagedPtr  #-}
    {-# INLINE mallocBytesIO     #-}

instance PtrType 'Memory.Managed where
    plus = \ptr -> wrap . Raw.plusForeignPtr (unwrap ptr)
    {-# INLINE plus #-}

    minus = \ (unwrap -> Raw.ForeignPtr !tgtAddr !_)
              (unwrap -> Raw.ForeignPtr !srcAddr !_)
            -> I# (minusAddr# tgtAddr srcAddr)
    {-# INLINE minus #-}

    nullPtr = wrap (coerce __foreignNullPtr)
    {-# INLINE nullPtr #-}

    unsafeToUnmanaged = wrap . Raw.unsafeForeignPtrToPtr . unwrap
    {-# INLINE unsafeToUnmanaged #-}

    withUnmanagedPtr = \ptr f -> do
        let fptr = unwrap ptr
        out <- f $! wrap $ Raw.unsafeForeignPtrToPtr fptr
        liftIO $ Raw.touchForeignPtr fptr
        pure out
    {-# INLINE withUnmanagedPtr #-}

    mallocBytesIO = fmap wrap . Raw.mallocForeignPtrBytes
    {-# INLINE mallocBytesIO #-}


-- === Utils === --

copyBytes :: (PtrType t, MonadIO m) => Ptr t a -> Ptr t a -> Int -> m ()
copyBytes = \tgt src size -> withUnmanagedRawPtr tgt
          $ \rtgt         -> withUnmanagedRawPtr src
          $ \rsrc         -> liftIO $ Raw.copyBytes rtgt rsrc size
{-# INLINE copyBytes #-}

moveBytes :: (PtrType t, MonadIO m) => Ptr t a -> Ptr t a -> Int -> m ()
moveBytes = \tgt src size -> withUnmanagedRawPtr tgt
          $ \rtgt         -> withUnmanagedRawPtr src
          $ \rsrc         -> liftIO $ Raw.moveBytes rtgt rsrc size
{-# INLINE moveBytes #-}

copyAndOffsetBytes :: (PtrType t, MonadIO m)
    => Ptr t a -> Ptr t a -> Int -> m (Ptr t b)
copyAndOffsetBytes = \tgt src siz -> (tgt `plus` siz) <$ copyBytes tgt src siz
{-# INLINE copyAndOffsetBytes #-}

withUnmanagedRawPtr :: (PtrType t, MonadIO m)
    => Ptr t a -> (Raw.Ptr a -> m b) -> m b
withUnmanagedRawPtr = \ptr f -> withUnmanagedPtr ptr (f . unwrap)
{-# INLINE withUnmanagedRawPtr #-}

mallocBytes :: ∀ t m a. PtrType t => MonadIO m => Int -> m (Ptr t a)
mallocBytes = liftIO . mallocBytesIO
{-# INLINE mallocBytes #-}

free :: MonadIO m => UnmanagedPtr a -> m ()
free = liftIO . Raw.free . unwrap
{-# INLINE free #-}

__foreignNullPtr :: Raw.ForeignPtr ()
__foreignNullPtr = unsafePerformIO $ Raw.newForeignPtr_ Raw.nullPtr
{-# NOINLINE __foreignNullPtr #-}

touchPtr :: MonadIO m => ManagedPtr a -> m ()
touchPtr = liftIO . Raw.touchForeignPtr . unwrap
{-# INLINE touchPtr #-}


-- === Instances === --

makeLenses ''Ptr
deriving instance Eq     (PtrImpl t a) => Eq     (Ptr t a)
deriving instance NFData (PtrImpl t a) => NFData (Ptr t a)
deriving instance Ord    (PtrImpl t a) => Ord    (Ptr t a)
deriving instance Show   (PtrImpl t a) => Show   (Ptr t a)
