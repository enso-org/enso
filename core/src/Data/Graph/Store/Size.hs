{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Size where

import Prologue

import qualified Data.Graph.Data.Component.Set    as Component
import qualified Data.Graph.Data.Component.Set    as ComponentSet
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Layer.Class      as Layer
import qualified Data.Graph.Fold.Class            as Fold
import qualified Data.Graph.Fold.Filter           as Fold
import qualified Data.Graph.Fold.Scoped           as Fold
import qualified Data.Graph.Fold.Struct           as Fold
import qualified Data.Graph.Store.MemoryRegion    as MemoryRegion
import qualified Foreign.DynamicStorable          as DynamicStorable
import qualified Foreign.Storable.Utils           as Storable

import Data.Graph.Data.Component.Class  (Component)
import Data.Graph.Data.Component.Set    (ComponentSet)
import Data.Graph.Data.Component.Vector (ComponentVector)
import Data.PtrSet.Mutable              (IsPtr, UnmanagedPtrSet)
import Data.Vector.Storable.Foreign     (Vector)
import Foreign.DynamicStorable          (DynamicStorable)
import Foreign.Ptr                      (Ptr, plusPtr)
import Foreign.Ptr.Utils                (SomePtr)
import Foreign.Storable.Utils           (Storable)



-- -------------------------
-- -- === External Size === --
-- -------------------------

-- -- === Definition === --

-- data Size = Size
--     { _staticSize  :: Int
--     , _dynamicSize :: DynamicSize
--     } deriving Show
-- makeLenses ''Size

-- data DynamicSize = DynamicSize
--     { _noPointerRegionByteSize :: Int
--     , _pointerRegionByteSize   :: Int
--     } deriving Show
-- makeLenses ''DynamicSize


-- -- === Instances === --

-- instance Mempty Size where
--     mempty = Size 0 0
--     {-# INLINE mempty #-}

-- instance Semigroup Size where
--     (<>) = \(Size nps1 ps1) (Size nps2 ps2) ->
--         let nps = nps1 + nps2
--             ps  = ps1  + ps2
--         in Size nps ps
--     {-# INLINE (<>) #-}


-- -- === Helpers === --

-- totalSize :: Size -> Int
-- totalSize = \(Size nps ps) -> nps + ps
-- {-# INLINE totalSize #-}

-- addSizeTo ::(DynamicStorable a, MonadIO m)
--            => Lens' Size Int -> a -> m Size -> m Size
-- addSizeTo = \l a accM -> do
--     size <- liftIO $! DynamicStorable.sizeOf a
--     acc  <- accM
--     pure $! acc <> (l .~ size $! mempty)
-- {-# INLINE addSizeTo #-}



-- ---------------------------
-- -- === SizeDiscovery === --
-- ---------------------------

-- data Discovery
-- type instance Fold.Result     Discovery = Size
-- type instance Fold.LayerScope Discovery = 'Fold.All
-- type SizeDiscoveryBuilder1 = Fold.Builder1 SizeDiscoveryCfg
-- type SizeDiscoveryCfg      = Fold.Filter Storable.Dynamics Storable.Dynamic
--                                          Discovery


-- -- === API === --

-- class SizeDiscovery  m a where size  ::       a    -> m Size
-- class SizeDiscovery1 m a where size1 :: ∀ t1. a t1 -> m Size

-- instance {-# OVERLAPPABLE #-} Fold.Builder SizeDiscoveryCfg m a
--       => SizeDiscovery m a where
--     size = \a -> Fold.build @SizeDiscoveryCfg a $ pure mempty
--     {-# INLINE size #-}

-- instance {-# OVERLAPPABLE #-} SizeDiscoveryBuilder1 m a
--       => SizeDiscovery1 m a where
--     size1 = size1__
--     {-# INLINE size1 #-}

-- instance {-# OVERLAPPABLE #-} SizeDiscovery1 m (Component comp)
--       => SizeDiscovery m (Component comp layout) where
--     size = size1
--     {-# INLINE size #-}

-- instance {-# OVERLAPPABLE #-}
--     Fold.Builder1 (Fold.Scoped Discovery) m (Component comp)
--       => SizeDiscovery1 m (Component comp) where
--     size1 = \a -> Fold.build1 @(Fold.Scoped Discovery) a $ pure mempty
--     {-# INLINE size1 #-}

-- size1__ :: SizeDiscoveryBuilder1 m a => a t1 -> m Size
-- size1__ = \a -> Fold.build1 @SizeDiscoveryCfg a $ pure mempty
-- {-# INLINE size1__ #-}


-- -- === Instances === --

-- instance (MonadIO m, SizeDiscoveryBuilder1 m (Layer.Cons layer))
--       => Fold.LayerBuilder Discovery m layer where
--     layerBuild = \a mi -> (<>) <$> mi <*> size1__ a
--     {-# INLINE layerBuild #-}

-- instance MonadIO m
--       => Fold.Builder1 Discovery m (ComponentSet comp) where
--     build1 = addSizeTo pointersSize . unwrap
--     {-# INLINE build1 #-}

-- instance MonadIO m
--       => Fold.Builder1 Discovery m (ComponentVector comp) where
--     build1 = addSizeTo pointersSize . unwrap
--     {-# INLINE build1 #-}

-- instance (MonadIO m, Storable a)
--       => Fold.Builder Discovery m (Vector a) where
--     build = addSizeTo noPointersSize
--     {-# INLINE build #-}

-- instance {-# OVERLAPPABLE #-}
--     (Monad m, Fold.Builder1 (Fold.Struct SizeDiscoveryCfg) m a)
--       => Fold.Builder1 Discovery m a where
--     build1 = Fold.build1 @(Fold.Struct SizeDiscoveryCfg)
--     {-# INLINE build1 #-}



-- ------------------------------
-- -- === ExternalStorable === --
-- ------------------------------

-- -- === Definition === --

-- -- | The 'load' and 'dump' functions allow loading and storing dynamic parts
-- --   of a structure to a given memory chunk.
-- class ExternalStorable a where
--     loadBuilder :: Ptr a -> IO MemoryRegion.Dynamic -> IO MemoryRegion.Dynamic
--     dumpBuilder :: Ptr a -> IO MemoryRegion.Dynamic -> IO MemoryRegion.Dynamic

--     loadBuilder = \_ -> id ; {-# INLINE loadBuilder #-}
--     dumpBuilder = \_ -> id ; {-# INLINE dumpBuilder #-}


-- -- === API === --

-- load :: (MonadIO m, ExternalStorable a) => Ptr a -> MemoryRegion.Dynamic -> m MemoryRegion.Dynamic
-- dump :: (MonadIO m, ExternalStorable a) => Ptr a -> MemoryRegion.Dynamic -> m MemoryRegion.Dynamic
-- load = \ptr -> liftIO . loadBuilder ptr . pure ; {-# INLINE load #-}
-- dump = \ptr -> liftIO . dumpBuilder ptr . pure ; {-# INLINE dump #-}


-- -- === Instances === --

-- instance ExternalStorable Bool
-- instance ExternalStorable Char
-- instance ExternalStorable Int
-- instance ExternalStorable Word16
-- instance ExternalStorable Word32
-- instance ExternalStorable Word64
-- instance ExternalStorable Word8
-- instance ExternalStorable (Component comp layout)



-- -----------------------------------
-- -- === ExternalFieldStorable === --
-- -----------------------------------

-- class ExternalFieldStorable a where
--     loadFieldBuilder :: IO (a, MemoryRegion.Dynamic) -> IO (a, MemoryRegion.Dynamic)
--     dumpFieldBuilder :: a -> IO MemoryRegion.Dynamic -> IO MemoryRegion.Dynamic

--     loadFieldBuilder = id       ; {-# INLINE loadFieldBuilder #-}
--     dumpFieldBuilder = \_ -> id ; {-# INLINE dumpFieldBuilder #-}


-- loadField :: ExternalFieldStorable a => a -> MemoryRegion.Dynamic -> IO (a, MemoryRegion.Dynamic)
-- dumpField :: ExternalFieldStorable a => a -> MemoryRegion.Dynamic -> IO MemoryRegion.Dynamic
-- loadField = \a dynReg -> loadFieldBuilder $ pure (a, dynReg) ; {-# INLINE loadField #-}
-- dumpField = \a dynReg -> dumpFieldBuilder a $ pure dynReg    ; {-# INLINE dumpField #-}

-- instance ExternalFieldStorable Bool
-- instance ExternalFieldStorable Char
-- instance ExternalFieldStorable Int
-- instance ExternalFieldStorable Word16
-- instance ExternalFieldStorable Word32
-- instance ExternalFieldStorable Word64
-- instance ExternalFieldStorable Word8
-- instance ExternalFieldStorable (Component comp layout)



-- -----------------------
-- -- === Instances === --
-- -----------------------

-- loadStorable :: (DynamicStorable a, Storable a)
--              => MemoryRegion.DynamicMemVariant
--              -> Ptr a -> IO MemoryRegion.Dynamic
--              -> IO MemoryRegion.Dynamic
-- loadStorable = \l ptr mdynReg -> do
--     dynReg <- mdynReg
--     let dynPtr = dynReg ^. l
--     a <- DynamicStorable.peek (coerce dynPtr)
--     Storable.poke ptr a
--     dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     pure $! set l dynPtr' dynReg
-- {-# INLINE loadStorable #-}

-- dumpStorable :: (DynamicStorable a, Storable a)
--              => MemoryRegion.DynamicMemVariant
--              -> Ptr a -> IO MemoryRegion.Dynamic
--              -> IO MemoryRegion.Dynamic
-- dumpStorable = \l ptr mdynReg -> do
--     dynReg <- mdynReg
--     let dynPtr = dynReg ^. l
--     a <- Storable.peek ptr
--     DynamicStorable.poke (coerce dynPtr) a
--     dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     pure $! set l dynPtr' dynReg
-- {-# INLINE dumpStorable #-}

-- loadStorableField :: (DynamicStorable a, Storable a)
--                   => MemoryRegion.DynamicMemVariant
--                   -> IO (a, MemoryRegion.Dynamic)
--                   -> IO (a, MemoryRegion.Dynamic)
-- loadStorableField = \l mdata -> do
--     (!_, !dynReg) <- mdata
--     let dynPtr = dynReg ^. l
--     a <- DynamicStorable.peek (coerce dynPtr)
--     dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     let dynReg' = set l dynPtr' dynReg
--     pure (a, dynReg')
-- {-# INLINE loadStorableField #-}

-- dumpStorableField :: (DynamicStorable a, Storable a)
--                   => MemoryRegion.DynamicMemVariant
--                   -> a -> IO MemoryRegion.Dynamic
--                   -> IO MemoryRegion.Dynamic
-- dumpStorableField = \l a mdynReg -> do
--     dynReg <- mdynReg
--     let dynPtr = dynReg ^. l
--     DynamicStorable.poke (coerce dynPtr) a
--     (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     dynPtr' <- (dynPtr `plusPtr`) <$> DynamicStorable.sizeOf a
--     pure $! set l dynPtr' dynReg
-- {-# INLINE dumpStorableField #-}

-- -- === UnmanagedPtrSet === --

-- instance (Storable a, IsPtr a) => ExternalStorable (UnmanagedPtrSet a) where
--     loadBuilder = loadStorable MemoryRegion.pointersMem
--     dumpBuilder = dumpStorable MemoryRegion.pointersMem
--     {-# INLINE loadBuilder #-}
--     {-# INLINE dumpBuilder #-}

-- instance ExternalFieldStorable (ComponentSet comp layout) where
--     loadFieldBuilder = loadStorableField MemoryRegion.pointersMem
--     dumpFieldBuilder = dumpStorableField MemoryRegion.pointersMem
--     {-# INLINE loadFieldBuilder #-}
--     {-# INLINE dumpFieldBuilder #-}


-- -- === Vector === --

-- instance Storable a => ExternalStorable (Vector a) where
--     loadBuilder = loadStorable MemoryRegion.noPointersMem
--     dumpBuilder = dumpStorable MemoryRegion.noPointersMem
--     {-# INLINE loadBuilder #-}
--     {-# INLINE dumpBuilder #-}

-- instance Storable a => ExternalFieldStorable (Vector a) where
--     loadFieldBuilder = loadStorableField MemoryRegion.noPointersMem
--     dumpFieldBuilder = dumpStorableField MemoryRegion.noPointersMem
--     {-# INLINE loadFieldBuilder #-}
--     {-# INLINE dumpFieldBuilder #-}

-- instance ExternalStorable (ComponentVector comp layout) where
--     loadBuilder = loadStorable MemoryRegion.pointersMem
--     dumpBuilder = dumpStorable MemoryRegion.pointersMem
--     {-# INLINE loadBuilder #-}
--     {-# INLINE dumpBuilder #-}

-- instance ExternalFieldStorable (ComponentVector comp layout) where
--     loadFieldBuilder = loadStorableField MemoryRegion.pointersMem
--     dumpFieldBuilder = dumpStorableField MemoryRegion.pointersMem
--     {-# INLINE loadFieldBuilder #-}
--     {-# INLINE dumpFieldBuilder #-}
