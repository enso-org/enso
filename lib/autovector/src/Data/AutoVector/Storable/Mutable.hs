module Data.AutoVector.Storable.Mutable where

-- import Prologue hiding (length)

-- import qualified Data.Vector.Storable         as PVector
-- import qualified Data.Vector.Storable.Mutable as Vector
-- import qualified Foreign.ForeignPtr.Utils     as Ptr

-- import Data.Vector.Storable         (Vector)
-- import Data.Vector.Storable.Mutable (MVector)
-- import Foreign.ForeignPtr           (ForeignPtr)
-- import Foreign.Storable             (Storable)


-- ------------------------------
-- -- === AutoVector Class === --
-- ------------------------------

-- class IsAutoVector v where
--     type family ContainerOf (v :: * -> *) :: * -> *
--     type family PtrOf       (v :: * -> *) (a :: *) :: *
--     dataVector  :: forall a. Lens' (v a) (ContainerOf v a)
--     indexVector :: forall a. Lens' (v a) (ContainerOf v Int)
--     unusedSize  :: forall a. Lens' (v a) (PtrOf       v Int)



-- ------------------------
-- -- === AutoVector === --
-- ------------------------

-- data AutoVector a = AutoVector
--     { __dataVector  :: {-# UNPACK #-} !(Vector a)
--     , __indexVector :: {-# UNPACK #-} !(Vector Int)
--     , __unusedSize  :: {-# UNPACK #-} !Int
--     } deriving (Generic, Show)
-- makeLenses ''AutoVector

-- instance IsAutoVector AutoVector where
--     type ContainerOf  AutoVector   = Vector
--     type PtrOf        AutoVector a = a
--     dataVector  = autoVector_dataVector  ; {-# INLINE dataVector  #-}
--     indexVector = autoVector_indexVector ; {-# INLINE indexVector #-}
--     unusedSize  = autoVector_unusedSize  ; {-# INLINE unusedSize  #-}



-- -------------------------
-- -- === MAutoVector === --
-- -------------------------

-- -- === Definition === --

-- type MAutoVector' m   = MAutoVector (PrimState m)
-- data MAutoVector  s a = MAutoVector
--     { __dataVector  :: {-# UNPACK #-} !(MVector s a)
--     , __indexVector :: {-# UNPACK #-} !(MVector s Int)
--     , __unusedSize  :: {-# UNPACK #-} !(ForeignPtr Int)
--     } deriving (Generic)
-- makeLenses ''MAutoVector

-- instance IsAutoVector (MAutoVector s) where
--     type ContainerOf  (MAutoVector s)   = MVector s
--     type PtrOf        (MAutoVector s) a = ForeignPtr a
--     dataVector  = mAutoVector_dataVector  ; {-# INLINE dataVector  #-}
--     indexVector = mAutoVector_indexVector ; {-# INLINE indexVector #-}
--     unusedSize  = mAutoVector_unusedSize  ; {-# INLINE unusedSize  #-}



-- -- === Construction === --

-- unsafeNew :: (MonadIO m, PrimMonad m, Storable a) => Int -> m (MAutoVector' m a)
-- unsafeNew !i = MAutoVector
--     <$> Vector.unsafeNew i
--     <*> PVector.unsafeThaw (PVector.generate i (maxKey-))
--     <*> Ptr.mkForeignPtr i
--     where maxKey = i - 1
-- {-# INLINE unsafeNew #-}



-- -- === Properties === --

-- -- | O(1)
-- length :: Storable a => MAutoVector s a -> Int
-- length v = Vector.length $ v ^. dataVector ; {-# INLINE length #-}

-- -- | O(1)
-- used, unused :: (MonadIO m, Storable a) => MAutoVector' m a -> m Int
-- used   v = (length v -) <$> unused v
-- unused v = Ptr.readForeignPtr (v ^. unusedSize)



-- -- === Conversions === --

-- unsafeThaw :: (MonadIO m, PrimMonad m, Storable a) => AutoVector a -> m (MAutoVector' m a)
-- unsafeThaw v = do
--     dataVector'  <- PVector.unsafeThaw $ v ^. dataVector
--     indexVector' <- PVector.unsafeThaw $ v ^. indexVector
--     unusedSize'  <- Ptr.mkForeignPtr   $ v ^. unusedSize
--     return $ MAutoVector dataVector' indexVector' unusedSize'
-- {-# INLINE unsafeThaw #-}

-- unsafeFreeze :: (MonadIO m, PrimMonad m, Storable a) => MAutoVector' m a -> m (AutoVector a)
-- unsafeFreeze v = do
--     dataVector'  <- PVector.unsafeFreeze $ v ^. dataVector
--     indexVector' <- PVector.unsafeFreeze $ v ^. indexVector
--     unusedSize'  <- Ptr.readForeignPtr   $ v ^. unusedSize
--     return $ AutoVector dataVector' indexVector' unusedSize'
-- {-# INLINE unsafeFreeze #-}



-- -- === Modifications === --

-- -- | O(1) / O(1) + memcpy
-- reserveIndex :: (MonadIO m, PrimMonad m) => MAutoVector' m a -> m Int
-- reserveIndex v = do
--     kid <- Ptr.mapAndGetForeignPtr (v ^. unusedSize) (subtract 1)
--     when_ (kid < 0) $ error "TODO: resize"
--     Vector.unsafeRead (v ^. indexVector) kid
-- {-# INLINE reserveIndex #-}

-- -- | O(1)
-- releaseIndex :: (MonadIO m, PrimMonad m) => MAutoVector' m a -> Int -> m ()
-- releaseIndex v i = do
--     kid <- Ptr.mapAndGetForeignPtr (v ^. unusedSize) (+1)
--     Vector.unsafeWrite (v ^. indexVector) kid i
-- {-# INLINE releaseIndex #-}


-- unsafeWrite :: (PrimMonad m, Storable a) => MAutoVector' m a -> Int -> a -> m ()
-- unsafeWrite v = Vector.unsafeWrite (v ^. dataVector)

-- -- pushBack :: MAutoVector' m a -> a -> m ()
-- -- pushBack v a = do
-- --     idx <- reserveIndex v


-- -- -- | Copy a vector with an offset. The source vectors must have the length of
-- -- --   the target vector - offset. This is not checked.
-- -- --
-- -- --   >>> unsafeCopyWithOffset 3 [0, 0, 0, 0, 0, 0] [1, 2, 3]
-- -- --   [0, 0, 0, 1, 2, 3]
-- -- unsafeCopyWithOffset :: (PrimMonad m, Storable a) => Int -> MVector (PrimState m) a -> MVector (PrimState m) a -> m ()
-- -- unsafeCopyWithOffset off dst src = do
-- --     dstView <- Vector.unsafeDrop off dst
-- --     Vector.unsafeCopy dstView src
-- -- {-# INLINE unsafeCopyWithOffset #-}


-- -- -- | grows the whole MAutoVector', taking into account the moving of indices
-- -- --
-- -- -- >>> let s = MAutoVector [x1, x2, x3, x4] [i1, i2, i3, i4] 0
-- -- -- >>> unsafeDoubleGrow s yields MAutoVector [x1, x2, x3, x4, _, _, _, _] [7,6,5,4] 4
-- -- unsafeDoubleGrow :: (MonadIO m, PrimMonad m, Storable a) => MAutoVector' m a -> m (MAutoVector' m a)
-- -- unsafeDoubleGrow v = do
-- --     let len  = length v
-- --         len' = 2 * len
-- --     dataVector' <- Vector.unsafeNew len'
-- --     indexVector'    <- Vector.unsafeNew len'
-- --
-- --     let dstView = Vector.unsafeTake len dataVector'
-- --     Vector.unsafeCopy dstView (v ^. vector)
-- --
-- --     -- TODO: fill indexVector'
-- --
-- --     return $ MAutoVector dataVector' indexVector' len


-- --
-- -- unsafeGrow :: (PrimMonad m, Storable a) => MAutoVector' m a -> Int -> m (MAutoVector' m a)
-- -- unsafeGrow !s !i = assert (i > 0) $ vector (flip Vector.unsafeGrow i) s ; {-# INLINE unsafeGrow #-} -- FIXME: alokacja pustych adresow!
-- --
-- -- unsafeDoubleGrow :: (PrimMonad m, Storable a) => MAutoVector' m a -> m (MAutoVector' m a)
-- -- unsafeDoubleGrow !s = unsafeGrow s (length s) ; {-# INLINE unsafeDoubleGrow #-}
-- --
-- -- unsafeWrite :: (PrimMonad m, Storable a, Convertible' k Int) => MAutoVector' m a -> k -> a -> m ()
-- -- unsafeWrite !s !k !a = Vector.unsafeWrite (s ^. vector) (convert' k) a ; {-# INLINE unsafeWrite #-}
-- --
-- -- unsafeWriteSpec :: forall t a k m. (PrimMonad m, Storable t, Convertible' k Int, t~a) => MAutoVector' m a -> k -> t -> m ()
-- -- unsafeWriteSpec !s !k !t = Vector.unsafeWrite (s ^. vector) (convert' k) t ; {-# INLINE unsafeWriteSpec #-}



-- -- === Instances === --


-- instance NFData (MAutoVector s a) where
--     rnf v = () where
--         !_ = rnf (v ^. dataVector)
--         !_ = rnf (v ^. indexVector)
