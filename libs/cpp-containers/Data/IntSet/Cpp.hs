module Data.IntSet.Cpp where

import Prologue hiding (toList)

import Control.Monad.IO.Class
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable       (Storable)
import Foreign.Utils          (fromCBool)
import System.IO.Unsafe       (unsafePerformIO)

-- foo :: Ptr () -> Int
-- foo = coerce
--------------------
-- === IntSet === --
--------------------

-- === Definition === --

newtype IntSet          = IntSet          (ForeignPtr UnmanagedIntSet)
newtype UnmanagedIntSet = UnmanagedIntSet (Ptr UnmanagedIntSet)
    deriving (Storable)
makeLenses ''IntSet
makeLenses ''UnmanagedIntSet

class IsIntSet s where
    newIO  :: IO s
    withIO :: s -> (UnmanagedIntSet -> IO a) -> IO a


-- === Foreign calls === --

foreign import ccall unsafe "c_set_create"
    c_createIntSet :: IO UnmanagedIntSet

foreign import ccall unsafe "c_set_insert"
    c_insert :: Int -> UnmanagedIntSet -> IO ()

foreign import ccall unsafe "c_set_member"
    c_member :: Int -> UnmanagedIntSet -> IO Int

foreign import ccall unsafe "c_set_delete"
    c_delete :: Int -> UnmanagedIntSet -> IO ()

-- FIXME[WD, PM]
-- Sometimes breakes GHC:
--
--   Compiling Luna.IR.Term.Core ( src/Luna/IR/Term/Core.hs, .stack-work/dist/x86_64-linux-tinfo6/Cabal-2.0.1.0/build/Luna/IR/Term/Core.o ) [TH]
--   ghc: panic! (the 'impossible' happened)
--   (GHC version 8.2.2 for x86_64-unknown-linux):
--     Loading temp shared object failed: /tmp/ghc19938_0/libghc_21.so: undefined symbol: c_set_delete_set
--
-- foreign import ccall unsafe "&c_set_delete_set"
--     c_deleteSet :: FunPtr (Ptr UnmanagedIntSet -> IO ())

foreign import ccall unsafe "c_set_to_list"
    c_toList :: Ptr Int -> UnmanagedIntSet -> IO ()

foreign import ccall unsafe "c_set_size"
    c_size :: UnmanagedIntSet -> IO Int

foreign import ccall unsafe "c_set_null"
    c_null :: UnmanagedIntSet -> IO Int


-- === Instances === --

instance Show IntSet where
    show = show . unsafePerformIO . toList

instance Show UnmanagedIntSet where
    show = show . unsafePerformIO . toList

instance IsIntSet UnmanagedIntSet where
    newIO        = c_createIntSet ; {-# INLINE newIO  #-}
    withIO !s !f = f s            ; {-# INLINE withIO #-}

instance IsIntSet IntSet where
    newIO = undefined -- do
    --     ptr        <- newIO @UnmanagedIntSet
    --     foreignPtr <- newForeignPtr c_deleteSet (unwrap ptr)
    --     return . wrap $ coerce foreignPtr
    -- {-# INLINE newIO #-}
    -- withIO !s !f = withForeignPtr (unwrap s) (f . wrap) ; {-# INLINE withIO #-}


-- === API === --

-- | A utility function for easily using the C calls that use `Ptr ()`
--   with the IntSet.
with :: (IsIntSet s, MonadIO m) => s -> (UnmanagedIntSet -> IO a) -> m a
with !s !f = liftIO $ withIO s f ; {-# INLINE with #-}

-- | A utility function to perform a foreign C call on the set and return
--   the resulting set.
map :: (IsIntSet s, MonadIO m)
    => s -> (UnmanagedIntSet -> IO a) -> m s
map s f = with s f >> return s ; {-# INLINE map #-}

-- | Creates an `std::set` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
new :: (IsIntSet s, MonadIO m) => m s
new = liftIO newIO ; {-# INLINE new #-}

new' :: IsIntSet s => s
new' = unsafePerformIO new ; {-# NOINLINE new' #-}

-- | Create a set with one element
singleton :: (IsIntSet s, MonadIO m) => Int -> m s
singleton e = do
    s <- new
    insert s e
    return s
{-# INLINE singleton #-}

-- | Insert a single element into the set.
insert :: (IsIntSet s, MonadIO m) => s -> Int -> m ()
insert s e = with s $ c_insert e ; {-# INLINE insert #-}

-- | Bulk-insert a list of elements into the set.
insertMany :: (IsIntSet s, MonadIO m) => s -> [Int] -> m ()
insertMany s es = mapM_ (insert s) es ; {-# INLINE insertMany #-}

-- | Check whether the set contains a given key.
--   Since the set can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: (IsIntSet s, MonadIO m) => s -> Int -> m Bool
member s e = fromCBool =<< with s (c_member e)
{-# INLINE member #-}

-- | Remove an element from the set, using `std::set::erase`.
delete :: (IsIntSet s, MonadIO m) => s -> Int -> m ()
delete s e = with s $ c_delete e
{-# INLINE delete #-}

-- | Return the number of elements in the set.
size :: (IsIntSet s, MonadIO m) => s -> m Int
size s = with s c_size ; {-# INLINE size #-}

null :: (IsIntSet s, MonadIO m) => s -> m Bool
null s = fromCBool =<< with s c_null ; {-# INLINE null #-}

-- | Get all of the elements from the set (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: (IsIntSet s, MonadIO m) => s -> m [Int]
toList s = do
    n <- size s
    with s $ \ptr ->
        allocaArray @Int n $ \arr -> do
            c_toList arr ptr
            peekArray n arr
{-# INLINE toList #-}

-- | Convert a list to the set.
fromList :: (IsIntSet s, MonadIO m) => [Int] -> m s
fromList es = do
    s <- new
    insertMany s es
    return s
{-# INLINE fromList #-}
