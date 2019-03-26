{-# LANGUAGE NoStrict #-}
{-# LANGUAGE NoStrictData #-}

module Foreign.Memory.Pool2 where

import Prologue hiding (extend)

import qualified Control.Concurrent.MVar   as MVar
import qualified Foreign
import qualified Foreign.ForeignPtr        as ForeignPtr
import qualified Foreign.ForeignPtr.Unsafe as ForeignPtr
-- import qualified Foreign.Marshal.Utils     as Ptr
-- import qualified Foreign.Memory.Manager    as Mgr
import qualified Foreign.Ptr               as Ptr

import Control.Concurrent.MVar (MVar)
-- import Data.Default
import Foreign                 (Ptr, Storable)
import Foreign.ForeignPtr      (ForeignPtr)
import Foreign.Ptr.Utils       (SomePtr)
-- import Foreign.Storable.Utils  (sizeOf')



newtype BlockSize = BlockSize Int
newtype ItemSize  = ItemSize  Int
makeLenses ''BlockSize
makeLenses ''ItemSize

instance Default BlockSize where def = BlockSize 2048



data State = State
    { _blockSize :: !Int
    , _itemSize  :: !Int
    , _freePtrs  :: ![SomePtr]
    , _chunks    :: ![ForeignPtr ()]
    }
makeLenses ''State

newtype MemPool a = MemPool (MVar State)
makeLenses ''MemPool


new :: MonadIO m => BlockSize -> ItemSize -> m (MemPool a)
new (BlockSize !bs) (ItemSize !is) = liftIO $ do
    !state <- extend $! State bs is [] []
    MemPool <$> MVar.newMVar state
{-# INLINE new #-}

extend :: MonadIO m => State -> m State
extend s = liftIO $ do
    let !iSize = s ^. itemSize
        !block = s ^. blockSize
        !size  = iSize * block
    !chunk    <- ForeignPtr.mallocForeignPtrBytes size
    let !head  = ForeignPtr.unsafeForeignPtrToPtr chunk
        !ptrs  = mkPtrs iSize head block
    pure $! s & freePtrs %~ (<> ptrs)
              & chunks   %~ (chunk :)
{-# INLINE extend #-}

mkPtrs :: Int -> SomePtr -> Int -> [SomePtr]
mkPtrs off ptr els = (ptr `Ptr.plusPtr`) . (off*) <$> [0 .. els - 1]
{-# INLINE mkPtrs #-}



alloc :: MonadIO m => MemPool a -> m SomePtr
alloc (MemPool !mvar) = liftIO $ do
    !state  <- MVar.takeMVar mvar
    !state' <- if null (state ^. freePtrs) then extend state else pure state
    case state' ^. freePtrs of
        []     -> error "impossible"
        (p:ps) -> do
            MVar.putMVar mvar $! state' & freePtrs .~ ps
            pure p
{-# INLINE alloc #-}



-- | Free the memory obtained by using `alloc`.
free :: MonadIO m => MemPool a -> SomePtr -> m ()
free (MemPool !_) !_ = pure () -- liftIO $ do
    -- !state <- MVar.takeMVar mvar
    -- MVar.putMVar mvar $! state & freePtrs %~ (ptr:)
{-# INLINE free #-}

-- | Delete the manager, freeing all of the memory it allocated.
delete :: MonadIO m => MemPool a -> m ()
delete _ = pure () ; {-# INLINE delete #-}






-- instance Typeable comp => Show (ComponentMemPool comp) where
--     showsPrec d (unwrap -> a) = showParen' d $ showString name . showsPrec' a
--         where name = (<> " ") $ unwords
--                    [ "ComponentMemPool"
--                    , '@' : show (typeRep @comp)
--                    ]





allocPtr :: forall a m. (MonadIO m, Storable a) => m (Ptr a)
allocPtr = liftIO Foreign.malloc ; {-# INLINE allocPtr #-}

allocBytes :: forall a m. MonadIO m => Int -> m (Ptr a)
allocBytes t = liftIO $ Foreign.mallocBytes t ; {-# INLINE allocBytes #-}

-- unsafeNull :: MemPool a
-- unsafeNull = wrap Mgr.unsafeNull ; {-# INLINE unsafeNull #-}

-- -- | Create a new memory manager.
-- new :: MonadIO m => BlockSize -- ^ the number of items to pre-allocate
--                  -> ItemSize  -- ^ the size of one item
--                  -> m (MemPool a) -- ^ a new manager instance
-- new (BlockSize bs) (ItemSize is) = wrap <$> Mgr.newManager bs is ; {-# INLINE new #-}

-- | Allocate an element using the provided manager instance.
--   Note that even though the function is polymorphic in its
--   return type, the size of the allocated piece of memory will
--   be based on the manager used as the argument.
-- alloc :: MonadIO m => MemPool a -> m SomePtr
-- alloc = Mgr.newItem . unwrap ; {-# INLINE alloc #-}

-- -- | Free the memory obtained by using `alloc`.
-- free :: MonadIO m => MemPool a -> SomePtr -> m ()
-- free = Mgr.deleteItem . unwrap ; {-# INLINE free #-}

-- -- | Delete the manager, freeing all of the memory it allocated.
-- delete :: MonadIO m => MemPool a -> m ()
-- delete = Mgr.deleteManager . unwrap ; {-# INLINE delete #-}


--
-- import Foreign.Marshal.Alloc (mallocBytes)
-- import Foreign (Ptr, nullPtr, plusPtr, castPtr)
-- import Foreign.Storable.Utils (sizeOf')
--
-- import Control.Monad.State hiding (MonadIO, liftIO, return, fail)
--
-- import Foreign (Storable, poke, peek)
-- --
-- -- newtype Block = Block Int deriving (Show)
-- -- makeLenses ''Block
-- --
-- --
-- -- instance Storable a => Storable (UniCore a) where
-- --     sizeOf    a = unwrap a ; {-# INLINE sizeOf    #-}
-- --     alignment _ = chunkSize     ; {-# INLINE alignment #-}
-- --     peek ptr = peek (intPtr ptr) >>= \case
-- --         0 -> UVar <$> peekByteOff ptr chunkSize
-- --         1 -> UAcc <$> peekByteOff ptr chunkSize
-- --         _ -> error "Unrecognized constructor"
-- --     {-# INLINE peek #-}
-- --     poke ptr = \case
-- --         UVar !a -> poke (intPtr ptr) 0 >> pokeByteOff ptr chunkSize a
-- --         UAcc !a -> poke (intPtr ptr) 1 >> pokeByteOff ptr chunkSize a
-- --     {-# INLINE poke #-}
--
-- type Ptr' = Ptr ()
--
-- -- data Block = Block
-- --     { _size     :: {-# UNPACK #-} !Int
-- --     , _startPtr :: {-# UNPACK #-} !Ptr'
-- --     } deriving (Show)
-- -- makeLenses ''Block
-- --
-- --
-- -- newBlock :: MonadIO m => Int -> m Block
-- -- newBlock s = liftIO $ Block s <$> mallocBytes s ; {-# INLINE newBlock #-}
-- --
--
-- data Pool = Pool
--     { _elemBytes :: {-# UNPACK #-} !Int
--     , _blockSize :: {-# UNPACK #-} !Int
--     , _blocksNum :: {-# UNPACK #-} !Int
--     , _blocks    :: ![Ptr']
--     , _freePtrs  :: ![Ptr']
--     } deriving (Show)
-- makeLenses ''Pool
--
-- emptyPool :: Int -> Int -> Pool
-- emptyPool elBytes blkSize = Pool elBytes blkSize 0 mempty mempty
-- {-# INLINE emptyPool #-}
--
--
--
-- type MemoryPoolT = StateT Pool
-- type MonadMemoryPool m = (MonadState Pool m, MonadIO m, MonadFail m)
--
--
--
-- evalMemoryPoolT :: Monad m => Int -> Int -> MemoryPoolT m a -> m a
-- evalMemoryPoolT elBytes blkSize = flip evalStateT (emptyPool elBytes blkSize)
--
-- allocBlock :: MonadMemoryPool m => m ()
-- allocBlock = do
--     pool <- get
--     let elSize = pool ^. elemBytes
--         els    = pool ^. blockSize
--     block <- liftIO $ mallocBytes (els * elSize)
--     let newPtrs = plusPtr block . (* elSize) <$> [0..(els - 1)]
--         pool'   = pool & blocksNum %~ (+1)
--                        & blocks    %~ (block:)
--                        & freePtrs  %~ (<> newPtrs)
--     put pool'
-- {-# INLINE allocBlock #-}
--
-- allocPtr :: MonadMemoryPool m => m Ptr'
-- allocPtr = allocPtr' (allocBlock >> allocPtr' allocErr) where
--     allocErr    = fail "Cannot allocate memory"
--     allocPtr' f = do
--         pool <- get
--         case pool ^. freePtrs of
--             (p:ps) -> put (pool & freePtrs .~ ps) >> pure p
--             []     -> f
-- {-# INLINE allocPtr #-}
--
-- test :: IO ()
-- test = evalMemoryPoolT 2 3 $ do
--     print =<< allocPtr
--     print =<< allocPtr
--     print =<< allocPtr
--     print =<< allocPtr
--     print =<< allocPtr
--     print =<< allocPtr
--     print =<< allocPtr
--     pool <- get
--     pprint pool
--
--
--
--
-- type MemoryPoolT2 = StateT Pool2
-- type MonadMemoryPool2 m = (MonadState Pool2 m, MonadIO m, MonadFail m)
--
-- data Pool2 = Pool2
--     { _elemSize2   :: {-# UNPACK #-} !Int
--     , _blockElems2 :: {-# UNPACK #-} !Int
--     , _blocksNum2  :: {-# UNPACK #-} !Int
--     , _blocks2     :: ![Ptr']
--     , _headPtr2    :: !Ptr'
--     } deriving (Show)
-- makeLenses ''Pool2
--
-- emptyPool2 :: Int -> Int -> Pool2
-- emptyPool2 elBytes blkSize = Pool2 elBytes blkSize 0 mempty nullPtr
-- {-# INLINE emptyPool2 #-}
--
--
-- ptrSize :: Int
-- ptrSize = sizeOf' @Ptr'
--
--
-- -- The 'initPtrChain' function initializes Memory Pool writing to each chunk
-- -- pointer to the address of the next chunk. Such linked list is used while
-- -- reserving pointers from the pool. Following table illustrates the process:
-- --
-- --                 |  elSize  |
-- --                 v          v
-- --         head    0x0       0x2       0x4       0x6
-- --        +-----+  +---------+---------+---------+----------+
-- -- init:  | 0x0 |  |0x2      |0x4      |0x6      |nullPtr   |
-- --        +-----+  +---------+---------+---------+----------+
-- -- alloc: | 0x4 |  | ...     |0x4      |0x6      |nullPtr   |
-- --        +-----+  +---------+---------+---------+----------+
-- -- alloc: | 0x6 |  | ...     |...      |0x6      |nullPtr   |
-- --        +-----+  +---------+---------+---------+----------+
-- -- free:  | 0x0 |  | 0x4     |...      |0x6      |nullPtr   |
-- --        +-----+  +---------+---------+---------+----------+
-- --
-- initPtrChain :: MonadIO m => Int -> Ptr a -> Int -> m ()
-- initPtrChain elSize headPtr elems = liftIO $ go headPtr (elems - 1) where
--     go ptr 0 = poke (castPtr ptr) nullPtr
--     go ptr i = do
--         let nptr = plusPtr ptr elSize
--         poke (castPtr ptr) nptr
--         go nptr (i - 1)
-- {-# INLINE initPtrChain #-}
--
--
-- allocAndReplacePoolBlock :: MonadMemoryPool2 m => m ()
-- allocAndReplacePoolBlock = do
--     -- pool <- get
--     -- let elSize    = pool ^. elemSize2
--     --     elems     = pool ^. blockElems2
--     --     blockSize = elems * elSize
--     -- block <- liftIO $ mallocBytes (elems * elSize)
--     -- liftIO $ poke (castPtr $ pool ^. headPtr2) block
--     -- initPtrChain elSize block elems
--     -- let pool' = pool & blocksNum2 %~ (+1)
--     --                  & blocks2    %~ (block:)
--     -- put pool'
--     return ()
-- {-# INLINE allocAndReplacePoolBlock #-}
-- -- --
-- --
-- --
-- -- type MemoryPoolT = StateT Pool
-- -- type MonadMemoryPool m = (MonadState Pool m, MonadIO m, MonadFail m)
-- --
-- --
-- --
-- evalMemoryPoolT2 :: Monad m => Int -> Int -> MemoryPoolT2 m a -> m a
-- evalMemoryPoolT2 elBytes blkSize = flip evalStateT (emptyPool2 elBytes blkSize)
-- --
-- -- allocBlock :: MonadMemoryPool m => m ()
-- -- allocBlock = do
-- --     pool <- get
-- --     let elSize = pool ^. elemBytes
-- --         els    = pool ^. blockSize
-- --     block <- liftIO $ mallocBytes (els * elSize)
-- --     let newPtrs = plusPtr block . (* elSize) <$> [0..(els - 1)]
-- --         pool'   = pool & blocksNum %~ (+1)
-- --                        & blocks    %~ (block:)
-- --                        & freePtrs  %~ (<> newPtrs)
-- --     put pool'
-- -- {-# INLINE allocBlock #-}
-- --
-- allocPtr2 :: MonadMemoryPool2 m => m Ptr'
-- allocPtr2 = allocPtr' (allocAndReplacePoolBlock >> allocPtr' allocErr) where
--     allocErr    = fail "Cannot allocate memory"
--     allocPtr' f = do
--         pool <- get
--         nPtr <- liftIO $ peek (castPtr $ pool ^. headPtr2)
--         if nPtr == nullPtr then f else do
--             nnPtr <- liftIO $ peek $ (castPtr nPtr :: Ptr Ptr')
--             liftIO $ poke (castPtr $ pool ^. headPtr2) nnPtr
--             return nPtr
-- {-# INLINE allocPtr2 #-}
--
-- test2 :: IO ()
-- test2 = evalMemoryPoolT2 2 3 $ do
--     allocAndReplacePoolBlock
--     -- print =<< allocPtr2
-- --     print =<< allocPtr
-- --     print =<< allocPtr
-- --     print =<< allocPtr
-- --     print =<< allocPtr
-- --     print =<< allocPtr
-- --     print =<< allocPtr
--     pool <- get
--     pprint pool
