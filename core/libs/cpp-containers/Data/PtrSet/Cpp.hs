module Data.PtrSet.Cpp where

import Prologue hiding (toList)

import Control.Monad.IO.Class
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Utils      (SomePtr, fromCBool)
import Foreign.Storable       (Storable)
import System.IO.Unsafe       (unsafePerformIO)


--------------------
-- === PtrSet === --
--------------------

-- === Definition === --

newtype UnmanagedPtrSet = UnmanagedPtrSet (Ptr UnmanagedPtrSet)
    deriving (Storable)
makeLenses ''UnmanagedPtrSet

class IsPtrSet s where
    newIO  :: IO s
    withIO :: s -> (UnmanagedPtrSet -> IO a) -> IO a


-- === Foreign calls === --

foreign import ccall unsafe "c_set_create"
    c_createPtrSet :: IO UnmanagedPtrSet

foreign import ccall unsafe "c_set_insert"
    c_insert :: SomePtr -> UnmanagedPtrSet -> IO ()

foreign import ccall unsafe "c_set_member"
    c_member :: SomePtr -> UnmanagedPtrSet -> IO Int

foreign import ccall unsafe "c_set_delete"
    c_delete :: SomePtr -> UnmanagedPtrSet -> IO ()

foreign import ccall "c_set_delete_set"
    c_deleteSet :: UnmanagedPtrSet -> IO ()

foreign import ccall unsafe "c_set_to_list"
    c_toList :: Ptr SomePtr -> UnmanagedPtrSet -> IO ()

foreign import ccall unsafe "c_set_size"
    c_size :: UnmanagedPtrSet -> IO Int

foreign import ccall unsafe "c_set_null"
    c_null :: UnmanagedPtrSet -> IO Int


-- === Instances === --

instance Show UnmanagedPtrSet where
    show = show . unsafePerformIO . toList

instance IsPtrSet UnmanagedPtrSet where
    newIO        = c_createPtrSet ; {-# INLINE newIO  #-}
    withIO !s !f = f s            ; {-# INLINE withIO #-}


-- === API === --

-- | A utility function for easily using the C calls that use `Ptr ()`
--   with the PtrSet.
with :: (IsPtrSet s, MonadIO m) => s -> (UnmanagedPtrSet -> IO a) -> m a
with !s !f = liftIO $ withIO s f ; {-# INLINE with #-}

-- | A utility function to perform a foreign C call on the set and return
--   the resulting set.
map :: (IsPtrSet s, MonadIO m)
    => s -> (UnmanagedPtrSet -> IO a) -> m s
map s f = with s f >> return s ; {-# INLINE map #-}

-- | Creates an `std::set` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
new :: (IsPtrSet s, MonadIO m) => m s
new = liftIO newIO ; {-# INLINE new #-}

new' :: IsPtrSet s => s
new' = unsafePerformIO new ; {-# NOINLINE new' #-}

free :: (IsPtrSet s, MonadIO m) => s -> m ()
free s = with s c_deleteSet ; {-# INLINE free #-}

-- | Create a set with one element
singleton :: (IsPtrSet s, MonadIO m) => SomePtr -> m s
singleton e = do
    s <- new
    insert s e
    return s
{-# INLINE singleton #-}

-- | Insert a single element into the set.
insert :: (IsPtrSet s, MonadIO m) => s -> SomePtr -> m ()
insert s e = with s $ c_insert e ; {-# INLINE insert #-}

-- | Bulk-insert a list of elements into the set.
insertMany :: (IsPtrSet s, MonadIO m) => s -> [SomePtr] -> m ()
insertMany s es = mapM_ (insert s) es ; {-# INLINE insertMany #-}

-- | Check whether the set contains a given key.
--   Since the set can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: (IsPtrSet s, MonadIO m) => s -> SomePtr -> m Bool
member s e = fromCBool =<< with s (c_member e)
{-# INLINE member #-}

-- | Remove an element from the set, using `std::set::erase`.
delete :: (IsPtrSet s, MonadIO m) => s -> SomePtr -> m ()
delete s e = with s $ c_delete e
{-# INLINE delete #-}

-- | Return the number of elements in the set.
size :: (IsPtrSet s, MonadIO m) => s -> m Int
size s = with s c_size ; {-# INLINE size #-}

null :: (IsPtrSet s, MonadIO m) => s -> m Bool
null s = fromCBool =<< with s c_null ; {-# INLINE null #-}

-- | Get all of the elements from the set (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: (IsPtrSet s, MonadIO m) => s -> m [SomePtr]
toList s = do
    n <- size s
    with s $ \ptr ->
        allocaArray @SomePtr n $ \arr -> do
            c_toList arr ptr
            peekArray n arr
{-# INLINE toList #-}

-- | Convert a list to the set.
fromList :: (IsPtrSet s, MonadIO m) => [SomePtr] -> m s
fromList es = do
    s <- new
    insertMany s es
    return s
{-# INLINE fromList #-}

