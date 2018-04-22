-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntList.Cpp
--
-- A Haskell wrapper for C++'s std::deque, providing fast front- and back-
-- insertion and fast random access. Expect operations like:
-- `head` and `last` to be constant-time.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.PtrList.Mutable where

import           Prologue hiding (fromList, length, mapM, null, toList,
                           unsafeHead, unsafeLast)
import qualified Prologue as P

import qualified Data.Construction as Data

import Control.Monad          ((<=<))
import Control.Monad.IO.Class
import Data.Maybe             (fromJust)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc  hiding (free)
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Utils
import Foreign.Storable       (Storable)
import System.IO.Unsafe       (unsafePerformIO)


type IsPtr a = BiConvertible' SomePtr a


---------------------------------------------------
-- === Newtype wrappers for the list pointers === --
---------------------------------------------------

-- === Definition === --

newtype UnmanagedPtrList a = UnmanagedPtrList SomePtr deriving (Storable, Eq)
makeLenses ''UnmanagedPtrList

class IsPtrList t where
    newIO  :: ∀ a. IO (t a)
    withIO :: ∀ a b. t a -> (UnmanagedPtrList a -> IO b) -> IO b


-- === Foreign calls === --

foreign import ccall unsafe "c_list_create"
    c_new :: IO (UnmanagedPtrList a)

foreign import ccall unsafe "c_list_push_front"
    c_pushFront :: SomePtr -> UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_push_back"
    c_pushBack :: SomePtr -> UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_pop_front"
    c_popFront :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_pop_back"
    c_popBack :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_at"
    c_at :: Int -> UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_front"
    c_front :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_back"
    c_back :: UnmanagedPtrList a -> IO SomePtr

foreign import ccall unsafe "c_list_delete_list"
    c_deleteList :: UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_to_list"
    c_toList :: Ptr SomePtr -> UnmanagedPtrList a -> IO ()

foreign import ccall unsafe "c_list_size"
    c_size :: UnmanagedPtrList a -> IO Int

foreign import ccall unsafe "c_list_null"
    c_null :: UnmanagedPtrList a -> IO Int


-- -- === API === --

-- | A utility function for easily using the C calls that use `Ptr ()` with the UnmanagedPtrList x.
with :: (IsPtrList t, MonadIO m) => t a -> (UnmanagedPtrList a -> IO b) -> m b
with s f = liftIO $ withIO s f ; {-# INLINE with #-}

-- | A utility for wrapping unsafe indexing calls
whenNonEmpty :: (IsPtrList t, MonadIO m) => (t a -> m b) -> t a -> m (Maybe b)
whenNonEmpty indexer t = null t >>= \case
    True  -> pure Nothing
    False -> Just <$> indexer t
{-# INLINE whenNonEmpty #-}

-- | Creates an `std::deque` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
new :: (IsPtrList t, MonadIO m) => m (t a)
new = liftIO newIO ; {-# INLINE new #-}

free :: (IsPtrList t, MonadIO m) => t a -> m ()
free t = with t c_deleteList ; {-# INLINE free #-}

-- | Prepend a single element to the front of the list (O(1))
pushFront :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> a -> m ()
pushFront t a = with t (c_pushFront $ convert' a) ; {-# INLINE pushFront #-}

-- | Prepend a single element to the front of the list (O(1))
pushBack :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> a -> m ()
pushBack t a = with t (c_pushBack $ convert' a) ; {-# INLINE pushBack #-}

unsafePopFront :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafePopFront t = convert' <$> with t c_popFront ; {-# INLINE unsafePopFront #-}

-- | Remove the first element of the list, returning it (O(1))
popFront :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
popFront = whenNonEmpty unsafePopFront ; {-# INLINE popFront #-}

unsafePopBack :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafePopBack t = convert' <$> with t c_popBack ; {-# INLINE unsafePopBack #-}

-- | Remove the last element of the list, returning it (O(1))
popBack :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
popBack = whenNonEmpty unsafePopBack ; {-# INLINE popBack #-}

-- | Peek the first element of the list (without removing)
head :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
head = whenNonEmpty unsafeHead ; {-# INLINE head #-}

unsafeHead :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafeHead t = convert' <$> with t c_front ; {-# INLINE unsafeHead #-}

-- | Peek the last element of the list (without removing)
last :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m (Maybe a)
last = whenNonEmpty unsafeLast ; {-# INLINE last #-}

unsafeLast :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m a
unsafeLast t = convert' <$> with t c_back ; {-# INLINE unsafeLast #-}

-- | Bulk-insert a list of elements into the list. (O(n))
insertMany :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> [a] -> m ()
insertMany t = mapM_ (pushBack t) ; {-# INLINE insertMany #-}

-- | Get an element under a given index.
--   Returns `Just v` if the value exists, `Nothing` otherwise.
index :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> Int -> m (Maybe a)
index t idx = do
    n <- length t
    if idx >= n
        then pure Nothing
        else Just . convert' <$> with t (c_at idx)
{-# INLINE index #-}

-- | Return the number of elements in the list.
length :: (IsPtrList t, MonadIO m) => t a -> m Int
length t = with t c_size ; {-# INLINE length #-}

null :: (IsPtrList t, MonadIO m) => t a -> m Bool
null t = fromCBool =<< with t c_null ; {-# INLINE null #-}

-- | Get all of the elements from the list as a pure haskell list.
--   Note: do not overuse this function, as it will not perform very well.
toList :: (IsPtrList t, MonadIO m, IsPtr a) => t a -> m [a]
toList t = do
    n <- length t
    with t (\ptr ->
        allocaArray @SomePtr n (\arr -> do
            c_toList arr ptr
            convert' <<$>> peekArray n arr))
{-# INLINE toList #-}

-- | Convert a pure haskell list to the c++'s std::list.
fromList :: (IsPtrList t, MonadIO m, IsPtr a) => [a] -> m (t a)
fromList es = do
    t <- new
    insertMany t es
    pure t
{-# INLINE fromList #-}


mapM :: (IsPtrList t, MonadIO m, IsPtr a, IsPtr b)
     => (a -> m b) -> t a -> m (t b)
mapM f t = fromList =<< P.mapM f =<< toList t ; {-# INLINE mapM #-}



-- === Instances ===  --

instance (IsPtr a, Show a) => Show (UnmanagedPtrList a) where
    show = show . unsafePerformIO . toList

instance IsPtrList UnmanagedPtrList where
    newIO        = c_new ; {-# INLINE newIO  #-}
    withIO !s !f = f s             ; {-# INLINE withIO #-}

instance MonadIO m => Data.Destructor1 m UnmanagedPtrList where
    destruct1 = free ; {-# INLINE destruct1 #-}
