-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntList.Cpp
--
-- A Haskell wrapper for C++'s std::deque, providing fast front- and back-
-- insertion and fast random access. Expect operations like:
-- `head` and `last` to be constant-time.
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
module Data.IntList.Cpp where

import Prelude hiding (head, init, length, null, tail)

import Control.Monad          ((<=<))
import Control.Monad.IO.Class
import Data.Maybe             (fromJust)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Ptr.Utils



---------------------------------------------------
-- === Newtype wrappers for the list pointers === --
---------------------------------------------------

newtype StdList = StdList { toRawList :: ForeignPtr StdList }

newtype RawListPtr = RawListPtr { toRawPtr :: Ptr RawListPtr }



---------------------------
-- === Foreign calls === --
---------------------------

foreign import ccall unsafe "c_list_create"
    c_createStdList :: IO RawListPtr

foreign import ccall unsafe "c_list_push_front"
    c_pushFront :: Int -> RawListPtr -> IO ()

foreign import ccall unsafe "c_list_push_back"
    c_pushBack :: Int -> RawListPtr -> IO ()

foreign import ccall unsafe "c_list_pop_front"
    c_popFront :: RawListPtr -> IO Int

foreign import ccall unsafe "c_list_pop_back"
    c_popBack :: RawListPtr -> IO Int

foreign import ccall unsafe "c_list_at"
    c_at :: Int -> RawListPtr -> IO Int

foreign import ccall unsafe "c_list_front"
    c_front :: RawListPtr -> IO Int

foreign import ccall unsafe "c_list_back"
    c_back :: RawListPtr -> IO Int

foreign import ccall unsafe "&c_list_delete_list"
    c_deleteList :: FunPtr (Ptr RawListPtr -> IO ())

foreign import ccall unsafe "c_list_to_list"
    c_toList :: Ptr Int -> RawListPtr -> IO ()

foreign import ccall unsafe "c_list_size"
    c_size :: RawListPtr -> IO Int

foreign import ccall unsafe "c_list_null"
    c_null :: RawListPtr -> IO Int



--------------------------------------------
-- === Haskell wrappers for C++ calls === --
--------------------------------------------

-- | A utility function for easily using the C calls that use `Ptr ()` with the StdList.
withStdList :: MonadIO m => StdList -> (RawListPtr -> IO a) -> m a
withStdList s f = liftIO $ withForeignPtr (toRawList s) (f . RawListPtr . castPtr)
{-# INLINE withStdList #-}

-- | A utility function to perform a foreign C call on the list and return the resulting list.
mapStdList :: MonadIO m => StdList -> (RawListPtr -> IO a) -> m StdList
mapStdList s f = withStdList s f >> return s
{-# INLINE mapStdList #-}

-- | A utility for wrapping unsafe indexing calls
wrapEmpty :: MonadIO m => (StdList -> m a) -> StdList -> m (Maybe a)
wrapEmpty indexer l = null l >>= \case
    True  -> return Nothing
    False ->  Just <$> indexer l
{-# INLINE wrapEmpty #-}

-- | Creates an `std::deque` object in C++ and attaches a finalizer.
--   The memory will be automatically deallocated by Haskell's GC.
createStdList :: MonadIO m => m StdList
createStdList = liftIO $ do
    ptr        <- c_createStdList
    foreignPtr <- newForeignPtr c_deleteList (toRawPtr ptr)
    return $ StdList $ castForeignPtr foreignPtr
{-# INLINE createStdList #-}

-- | Create an empty Cpp IntList
empty :: MonadIO m => m StdList
empty = createStdList
{-# INLINE empty #-}

-- | Prepend a single element to the front of the list (O(1))
pushFront :: MonadIO m => StdList -> Int -> m ()
pushFront l e = withStdList l (c_pushFront e)
{-# INLINE pushFront #-}

-- | Prepend a single element to the front of the list (O(1))
pushBack :: MonadIO m => StdList -> Int -> m ()
pushBack l e = withStdList l (c_pushBack e)
{-# INLINE pushBack #-}

popFrontUnsafe :: MonadIO m => StdList -> m Int
popFrontUnsafe l = withStdList l c_popFront
{-# INLINE popFrontUnsafe #-}

-- | Remove the first element of the list, returning it (O(1))
popFront :: MonadIO m => StdList -> m (Maybe Int)
popFront = wrapEmpty popFrontUnsafe
{-# INLINE popFront #-}

popBackUnsafe :: MonadIO m => StdList -> m Int
popBackUnsafe l = withStdList l c_popBack
{-# INLINE popBackUnsafe #-}

-- | Remove the last element of the list, returning it (O(1))
popBack :: MonadIO m => StdList -> m (Maybe Int)
popBack = wrapEmpty popBackUnsafe
{-# INLINE popBack #-}

-- | Peek the first element of the list (without removing)
front :: MonadIO m => StdList -> m (Maybe Int)
front = wrapEmpty (\l -> withStdList l c_front)
{-# INLINE front #-}

frontUnsafe :: MonadIO m => StdList -> m Int
frontUnsafe l = fromJust <$> front l
{-# INLINE frontUnsafe #-}

-- | Peek the last element of the list (without removing)
back :: MonadIO m => StdList -> m (Maybe Int)
back = wrapEmpty (\l -> withStdList l c_back)
{-# INLINE back #-}

backUnsafe :: MonadIO m => StdList -> m Int
backUnsafe l = fromJust <$> back l
{-# INLINE backUnsafe #-}

-- | Bulk-insert a list of elements into the list. (O(n))
insertMany :: MonadIO m => StdList -> [Int] -> m ()
insertMany l = mapM_ (pushBack l)
{-# INLINE insertMany #-}

-- | Get an element under a given index.
--   Returns `Just v` if the value exists, `Nothing` otherwise.
index :: MonadIO m => StdList -> Int -> m (Maybe Int)
index l idx = do
    n <- length l
    if idx >= n
        then return Nothing
        else Just <$> withStdList l (c_at idx)
{-# INLINE index #-}

-- | Return the number of elements in the list.
length :: MonadIO m => StdList -> m Int
length l = withStdList l c_size
{-# INLINE length #-}

null :: MonadIO m => StdList -> m Bool
null l = fromCBool =<< withStdList l c_null
{-# INLINE null #-}

-- | Get all of the elements from the list as a pure haskell list.
--   Note: do not overuse this function, as it will not perform very well.
toList :: MonadIO m => StdList -> m [Int]
toList l = do
    n <- length l
    withStdList l (\ptr ->
        allocaArray @Int n (\arr -> do
            c_toList arr ptr
            peekArray n arr))
{-# INLINE toList #-}

-- | Convert a pure haskell list to the c++'s std::list.
fromList :: MonadIO m => [Int] -> m StdList
fromList es = do
    l <- empty
    insertMany l es
    return l
{-# INLINE fromList #-}

-- | Deconstruct the list into the first element and the rest.
--   Safe call: will return Maybe.
uncons :: MonadIO m => StdList -> m (Maybe (Int, [Int]))
uncons l = do
    -- TODO[piotrMocz]: it's not necessary to compute `tl` if hd is `Nothing`.
    hd <- popFront l
    tl <- Just <$> toList l
    return $ (,) <$> hd <*> tl
{-# INLINE uncons #-}

init, tail :: MonadIO m => StdList -> m StdList
init l = popBack l >> return l   ; {-# INLINE init #-}
tail l = popFront l >> return l  ; {-# INLINE tail #-}

initSafe, tailSafe :: MonadIO m => StdList -> m (Maybe StdList)
initSafe = wrapEmpty init        ; {-# INLINE initSafe #-}
tailSafe = wrapEmpty tail        ; {-# INLINE tailSafe #-}

---------------------
-- === Aliases === --
---------------------

cons :: MonadIO m => Int -> StdList -> m StdList
cons el l = pushFront l el >> return l
{-# INLINE cons #-}

head, last :: MonadIO m => StdList -> m Int
head = frontUnsafe ;  {-# INLINE head #-}
last = backUnsafe  ;  {-# INLINE last #-}

headSafe, lastSafe :: MonadIO m => StdList -> m (Maybe Int)
headSafe = front   ;  {-# INLINE headSafe #-}
lastSafe = back    ;  {-# INLINE lastSafe #-}
