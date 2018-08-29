{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Data.VectorSet.Mutable.Storable
    (module Data.VectorSet.Mutable.Storable, module X) where
import Data.Mutable.Class as X

-- import Prologue hiding (FromList, Read, ToList, empty, fromList, length, toList,
--                  unsafeRead)

-- import qualified Data.AutoVector.Mutable.Storable as Vector
-- import qualified Data.Construction                as Data
-- import qualified Data.List                        as List
-- import qualified Data.Storable                    as Struct
-- import qualified Foreign.Marshal.Alloc            as Mem
-- import qualified Foreign.Marshal.Utils            as Mem
-- import qualified Foreign.Storable.Deriving        as Storable
-- import qualified Foreign.Storable.Utils           as Storable

-- import Data.Storable          (type (-::), Struct)
-- import Foreign.Ptr            (Ptr, nullPtr, plusPtr)
-- import Foreign.Storable       (Storable)
-- import Foreign.Storable.Utils (castPeekAndOffset, castPokeAndOffset)


-- --------------------
-- -- === Vector === --
-- --------------------

-- -- === Definition === --

-- newtype VectorSet a = VectorSet (Vector a) deriving (NFData)
-- makeLenses ''VectorSet

-- type instance Item (VectorSet a) = a



-- -- === API === --

-- insert :: (MonadIO m, Ord a, Storable a) => VectorSet a -> a -> m ()
-- insert = \s a -> withElemEnv (\_ -> pure ())
--                  (\ix -> Vector.insert (unwrap s) ix a) s a
-- {-# INLINE insert #-}

-- remove :: (MonadIO m, Ord a, Storable a) => VectorSet a -> a -> m ()
-- remove = \s a -> withElemEnv (Vector.remove (unwrap s))
--                  (\_ -> pure ()) s a
-- {-# INLINE remove #-}


-- -- === Instances === --

-- instance MonadIO m
--       => Length m (VectorSet a) where
--     length = length . unwrap
--     {-# INLINE length #-}

-- withElemEnv :: (MonadIO m, Ord a, Storable a)
--     => (Int -> m ()) -> (Int -> m ()) -> VectorSet a -> a -> m ()
-- withElemEnv = \ff fe s a -> do
--     len <- length s
--     if len == 0 then fe 0
--     else let min = 0
--              max = len - 1
--              ix  = max `quot` 2
--          in  withElemEnv__ ff fe s a min ix max
-- {-# INLINE withElemEnv #-}

-- withElemEnv__ :: (MonadIO m, Ord a, Storable a)
--     => (Int -> m ()) -> (Int -> m ())
--     -> VectorSet a -> a -> Int -> Int -> Int -> m ()
-- withElemEnv__ ff fe s a = go where
--     go = \min ix max -> do
--         ixVal <- unsafeRead (unwrap s) ix
--         if max <= min then
--              if      a < ixVal then fe ix
--              else if a > ixVal then fe (ix + 1)
--              else                   ff  ix
--         else if a < ixVal then
--                  let max' = ix - 1
--                      ix'  = (min + max') `quot` 2
--                  in  go min ix' max'
--              else if a > ixVal then
--                  let min' = ix + 1
--                      ix'  = (min' + max) `quot` 2
--                  in  go min' ix' max
--              else ff ix
--     {-# INLINABLE go #-}
-- {-# INLINE withElemEnv__ #-}


-- instance (MonadIO m, Storable a)
--       => Read m (VectorSet a) where
--     unsafeRead = unsafeRead . unwrap
--     {-# INLINE unsafeRead #-}

-- instance (MonadIO m, Storable a)
--       => Write m (VectorSet a) where
--     unsafeWrite = unsafeWrite. unwrap
--     {-# INLINE unsafeWrite #-}

-- -- instance (MonadIO m, Storable a)
-- --       => FromList m (VectorSet a) where
-- --     fromList = \lst -> liftIO $ do
-- --         a <- new $! List.length lst
-- --         mapM_ (pushBack a) lst
-- --         pure a
-- --     {-# INLINE fromList #-}

-- instance (MonadIO m, Storable a)
--       => ToList m (VectorSet a) where
--     toList = \a -> do
--         len <- length a
--         mapM (unsafeRead a) [0 .. len - 1]
--     {-# INLINE toList #-}


-- -- === Debug === --

-- instance (Show a, Storable a) => Show (VectorSet a) where
--     show = show . unwrap




-- test :: IO ()
-- test = do
--     -- (v :: Vector Int) <- Vector.fromList [1]
--     -- let s = VectorSet v
--     -- print s
--     -- insert s 7
--     -- print s
--     -- insert s 3
--     -- print s
--     -- insert s 5
--     -- print s
--     -- insert s 12
--     -- print s
--     -- insert s 18
--     -- print s
--     -- insert s 2
--     -- print s
--     print "hello test"

-- -- min ix max
-- -- 0   2  5


-- -- instance MonadIO m => Size m (VectorSet a) where
-- --     size = Struct.readField _size
-- --     {-# INLINE size #-}

-- -- instance (MonadIO m, Storable a)
-- --       => New m (VectorSet a) where
-- --     new = \size -> liftIO $ do
-- --         let elemsByteSize = size * Storable.sizeOf' @a
-- --         elemsPtr <- liftIO $ Mem.mallocBytes elemsByteSize
-- --         Struct.construct @(VectorSet a) 0 size elemsPtr
-- --     {-# INLINE new #-}

-- -- instance MonadIO m
-- --       => Free m (VectorSet a) where
-- --     free = \a -> liftIO $ do
-- --         elemsPtr <- Struct.readField _elemsPtr a
-- --         Mem.free elemsPtr
-- --         Struct.free a
-- --     {-# INLINE free #-}

-- -- instance (MonadIO m, Storable a)
-- --       => Read m (VectorSet a) where
-- --     unsafeRead = \a ix -> liftIO $ do
-- --         elemsPtr <- Struct.readField _elemsPtr a
-- --         Storable.peekElemOff elemsPtr ix
-- --     {-# INLINE unsafeRead #-}

-- -- instance (MonadIO m, Storable a)
-- --       => Write m (VectorSet a) where
-- --     unsafeWrite = \a ix val -> liftIO $ do
-- --         elemsPtr <- Struct.readField _elemsPtr a
-- --         Storable.pokeElemOff elemsPtr ix val
-- --     {-# INLINE unsafeWrite #-}

-- -- instance (MonadIO m, Storable a)
-- --       => FromList m (VectorSet a) where
-- --     fromList = \lst -> liftIO $ do
-- --         a <- new $! List.length lst
-- --         mapM_ (pushBack a) lst
-- --         pure a
-- --     {-# INLINE fromList #-}

-- -- instance (MonadIO m, Storable a)
-- --       => ToList m (VectorSet a) where
-- --     toList = \a -> do
-- --         len <- length a
-- --         mapM (unsafeRead a) [0 .. len - 1]
-- --     {-# INLINE toList #-}

-- -- instance (MonadIO m, Storable a)
-- --       => Grow m (VectorSet a) where
-- --     grow = \a -> liftIO $ do
-- --         oldSize   <- size   a
-- --         elemCount <- length a
-- --         elemsPtr  <- Struct.readField _elemsPtr a
-- --         let newSize       = oldSize * 2
-- --             elemByteSize  = Storable.sizeOf' @a
-- --             bytesToMalloc = elemByteSize * newSize
-- --             bytesToCopy   = elemByteSize * elemCount
-- --         newElemsPtr <- Mem.mallocBytes bytesToMalloc
-- --         Mem.copyBytes newElemsPtr elemsPtr bytesToCopy
-- --         Mem.free elemsPtr
-- --         Struct.writeField _size     a newSize
-- --         Struct.writeField _elemsPtr a newElemsPtr
-- --     {-# INLINE grow #-}

-- -- instance (MonadIO m, Storable a)
-- --       => PushBack m (VectorSet a) where
-- --     pushBack = \a v -> liftIO $ do
-- --         len <- length a
-- --         siz <- size   a
-- --         when (len == siz) $ grow a
-- --         unsafeWrite a len v
-- --         Struct.writeField _length a (len + 1)
-- --     {-# INLINE pushBack #-}

