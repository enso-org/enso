module OCI.Pass.Sheduler where

import Prologue

-- import qualified Control.Monad.State.Layered as State
-- import qualified Data.Map                    as Map
-- import qualified Data.TypeMap.Strict         as TypeMap
-- import qualified Foreign.Marshal.Alloc       as Mem
-- import qualified Foreign.Marshal.Utils       as Mem
-- import qualified Foreign.Memory.Pool         as MemPool
-- import qualified Foreign.Ptr                 as Ptr
-- import qualified OCI.Pass.Registry           as Registry
-- import qualified Type.Data.List              as List

-- import Control.Monad.Exception     (Throws, throw)
-- import Control.Monad.State.Layered (StateT)
-- import Data.Map.Strict             (Map)
-- import Data.TypeMap.Strict         (TypeMap)
-- import Foreign.Memory.Pool         (MemPool)
-- import Foreign.Ptr.Utils           (SomePtr)



-- --------------------------
-- -- === StaticConfig === --
-- --------------------------

-- -- | StaticConfig is computed once after all components and primitive layers
-- --   have been registered. It encodes information about memory layout which
-- --   is constant trough all of compiler passes.


-- -- === Definition === --

-- newtype StaticConfig = StaticConfig
--     { _components :: Map SomeTypeRep ComponentStaticConfig
--     } deriving (Show)

-- data ComponentStaticConfig = ComponentStaticConfig
--     { _byteSize  :: !Int
--     , _layers    :: !(Map SomeTypeRep LayerStaticConfig)
--     , _layerInit :: !SomePtr
--     , _memPool   :: !MemPool
--     } deriving (Show)

-- newtype LayerStaticConfig = LayerStaticConfig
--     { _byteOffset :: Int
--     } deriving (Show)

-- makeLenses ''LayerStaticConfig
-- makeLenses ''ComponentStaticConfig
-- makeLenses ''StaticConfig


-- -- === Construction === --

-- computeStaticConfig :: MonadIO m => Registry.Registry -> m StaticConfig
-- computeStaticConfig cfg = StaticConfig
--                       <$> mapM computeComponentConfig
--                         ( cfg ^. Registry.components )
-- {-# INLINE computeStaticConfig #-}

-- computeComponentConfig :: MonadIO m
--                        => Registry.ComponentInfo -> m ComponentStaticConfig
-- computeComponentConfig compCfg = compInfo where
--     layerReps    = Map.keys  $ compCfg ^. Registry.layers
--     layerInfos   = Map.elems $ compCfg ^. Registry.layers
--     layerSizes   = view Registry.byteSize <$> layerInfos
--     layerOffsets = scanl (+) 0 layerSizes
--     layerCfgs    = LayerStaticConfig <$> layerOffsets
--     compSize     = sum layerSizes
--     compInfo     = ComponentStaticConfig compSize
--                <$> pure (fromList $ zip layerReps layerCfgs)
--                <*> prepareLayerInitializer layerInfos
--                <*> MemPool.new def (MemPool.ItemSize compSize)
-- {-# INLINE computeComponentConfig #-}

-- prepareLayerInitializer :: MonadIO m => [Registry.LayerInfo] -> m SomePtr
-- prepareLayerInitializer ls = do
--     ptr <- mallocLayerInitializer ls
--     fillLayerInitializer ptr ls
--     pure ptr

-- mallocLayerInitializer :: MonadIO m => [Registry.LayerInfo] -> m SomePtr
-- mallocLayerInitializer = \case
--     [] -> pure Ptr.nullPtr
--     ls -> liftIO . Mem.mallocBytes . sum $ view Registry.byteSize <$> ls

-- fillLayerInitializer :: MonadIO m => SomePtr -> [Registry.LayerInfo] -> m ()
-- fillLayerInitializer ptr = liftIO . \case
--     []     -> pure ()
--     (l:ls) -> Mem.copyBytes ptr (l ^. Registry.defPtr) (l ^. Registry.byteSize)
--            >> fillLayerInitializer (Ptr.plusPtr ptr (l ^. Registry.byteSize)) ls




