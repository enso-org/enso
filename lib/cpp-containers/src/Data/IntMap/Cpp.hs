{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.IntMap.Cpp where

import Prelude

import Control.Monad          ((<=<))
import Control.Monad.IO.Class
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Utils
import Foreign.Storable       (peek)


---------------------------------------------------
-- === Newtype wrappers for the map pointers === --
---------------------------------------------------

newtype StdMap = StdMap { toRawMap :: ForeignPtr StdMap }

newtype RawMapPtr = RawMapPtr { toRawPtr :: Ptr RawMapPtr }



---------------------------
-- === Foreign calls === --
---------------------------

foreign import ccall unsafe "c_map_create"
    c_createStdMap :: IO RawMapPtr

foreign import ccall unsafe "c_map_insert"
    c_insert :: Int -> Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "c_map_member"
    c_member :: Int -> RawMapPtr -> IO Int

foreign import ccall unsafe "c_map_lookup"
    c_lookup :: Int -> Ptr Int -> RawMapPtr -> IO Int

foreign import ccall unsafe "c_map_delete"
    c_delete :: Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "&c_map_delete_map"
    c_deleteMap :: FunPtr (Ptr RawMapPtr -> IO ())

foreign import ccall unsafe "c_map_to_list"
    c_toList :: Ptr Int -> Ptr Int -> RawMapPtr -> IO ()

foreign import ccall unsafe "c_map_size"
    c_size :: RawMapPtr -> IO Int

foreign import ccall unsafe "c_map_null"
    c_null :: RawMapPtr -> IO Int



--------------------------------------------
-- === Haskell wrappers for C++ calls === --
--------------------------------------------

-- | A utility function for easily using the C calls that use `Ptr ()` with the StdMap.
withStdMap :: MonadIO m => StdMap -> (RawMapPtr -> IO a) -> m a
withStdMap s f = liftIO $ withForeignPtr (toRawMap s) (f . RawMapPtr . castPtr)
{-# INLINE withStdMap #-}

-- | A utility function to perform a foreign C call on the map and return the resulting map.
mapStdMap :: MonadIO m => StdMap -> (RawMapPtr -> IO a) -> m StdMap
mapStdMap s f = withStdMap s f >> return s
{-# INLINE mapStdMap #-}

-- | Creates an `std::map` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
createStdMap :: MonadIO m => m StdMap
createStdMap = liftIO $ do
    ptr        <- c_createStdMap
    foreignPtr <- newForeignPtr c_deleteMap (toRawPtr ptr)
    return . StdMap $ castForeignPtr foreignPtr
{-# INLINE createStdMap #-}

-- | Create an empty Cpp PtrMap
empty :: MonadIO m => m StdMap
empty = createStdMap
{-# INLINE empty #-}

-- | Create a map with one element (one key and one value)
singleton :: MonadIO m => Int -> Int -> m StdMap
singleton k v = do
    m <- empty
    insert m k v
    return m
{-# INLINE singleton #-}

-- | Insert a single element into the map.
insert :: MonadIO m => StdMap -> Int -> Int -> m ()
insert m k v = withStdMap m (c_insert k v)
{-# INLINE insert #-}

-- | Bulk-insert a list of pairs into the map.
insertMany :: MonadIO m => StdMap -> [(Int, Int)] -> m ()
insertMany m ps = mapM_ (uncurry $ insert m) ps
{-# INLINE insertMany #-}

-- | Check whether the map contains a given key.
--   Since the map can only contain 'Int' values,
--   This is also equivalent to the `find` function.
member :: MonadIO m => StdMap -> Int -> m Bool
member m k = fromCBool =<< withStdMap m (c_member k)
{-# INLINE member #-}

-- | Try to find the element in the map, returning Maybe elem.
lookup :: MonadIO m => StdMap -> Int -> m (Maybe Int)
lookup m k = withStdMap m $ \ptr ->
    alloca @Int $ \res -> do
        exists <- fromCBool =<< c_lookup k res ptr
        if exists then Just <$> peek res else return Nothing
{-# INLINE lookup #-}

-- | Remove an element under a given key from the map, using `std::map::erase`.
delete :: MonadIO m => StdMap -> Int -> m ()
delete m k = withStdMap m (c_delete k)
{-# INLINE delete #-}

-- | Return the number of elements in the map.
size :: MonadIO m => StdMap -> m Int
size m = withStdMap m c_size
{-# INLINE size #-}

-- | Is the map empty?
null :: MonadIO m => StdMap -> m Bool
null m = fromCBool =<< withStdMap m c_null
{-# INLINE null #-}

-- | Get all of the elements from the map (in ascending order).
--   Note: do not overuse this function, as it will not perform very well.
toList :: MonadIO m => StdMap -> m [(Int, Int)]
toList m = do
    n <- size m
    withStdMap m $ \ptr ->
        allocaArray @Int n $ \ks -> do
        allocaArray @Int n $ \vs -> do
            c_toList ks vs ptr
            zip <$> peekArray n ks <*> peekArray n vs
{-# INLINE toList #-}

-- | Convert a list to the map.
fromList :: MonadIO m => [(Int, Int)] -> m StdMap
fromList ps = do
    m <- empty
    insertMany m ps
    return m
{-# INLINE fromList #-}
