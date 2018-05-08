module OCI.Pass.State.IRInfo where

import Prologue

import qualified Data.Graph.Component.Class   as Component
import qualified Data.Graph.Component.Dynamic as Component
import qualified Data.Map                     as Map
import qualified Foreign.Marshal.Alloc        as Mem
import qualified Foreign.Marshal.Utils        as Mem
import qualified Foreign.Memory.Pool          as MemPool
import qualified Foreign.Ptr                  as Ptr

import Data.Map.Strict     (Map)
import Foreign.Memory.Pool (MemPool)
import Foreign.Ptr.Utils   (SomePtr)



------------------
-- === Info === --
------------------

-- === RegistryInfo === --

newtype Info = Info
    { _components :: Map Component.TagRep ComponentInfo
    } deriving (Default)

newtype ComponentInfo = ComponentInfo
    { _layers :: Map SomeTypeRep LayerInfo
    } deriving (Default, Mempty, Semigroup)

data LayerInfo = LayerInfo
    { _layerByteSize :: !Int
    , _initializer   :: !(Maybe SomePtr)
    , _constructor   :: !(Maybe (SomePtr -> IO ()))
    , _destructor    :: !(Maybe (SomePtr -> IO ()))
    , _subComponents :: !(SomePtr -> IO [Component.Dynamic])
    }

makeLenses ''Info
makeLenses ''ComponentInfo
makeLenses ''LayerInfo


-- === RuntimeInfo === --

newtype CompiledInfo = CompiledInfo
    { _compiledComponents :: Map Component.TagRep ComponentCompiledInfo
    }

data ComponentCompiledInfo = ComponentCompiledInfo
    { _compiledLayers    :: !(Map SomeTypeRep LayerCompiledInfo)
    , _layersByteSize    :: !Int
    , _layersInitializer :: !SomePtr
    , _layersConstructor :: !(SomePtr -> IO ())
    , _layersDestructor  :: !(SomePtr -> IO ())
    , _layersComponents  :: !(SomePtr -> IO [Component.Dynamic])
    , _memPool           :: !(MemPool ())
    }

newtype LayerCompiledInfo = LayerCompiledInfo
    { _byteOffset :: Int
    } deriving (Show)

makeLenses ''CompiledInfo
makeLenses ''ComponentCompiledInfo
makeLenses ''LayerCompiledInfo


-- === API === --

compile :: MonadIO m => Info -> m CompiledInfo
compile cfg = wrap <$> mapM computeComponentInfo (cfg ^. components) ; {-# INLINE compile #-}

computeComponentInfo :: MonadIO m => ComponentInfo -> m ComponentCompiledInfo
computeComponentInfo compCfg = compInfo where
    layerReps     = Map.keys  $ compCfg ^. layers
    layerInfos    = Map.elems $ compCfg ^. layers
    layerSizes    = view layerByteSize <$> layerInfos
    layerOffsets  = scanl (+) 0 layerSizes
    layerCfgs     = wrap <$> layerOffsets
    layerOffInfos = zip layerOffsets layerInfos
    compSize      = sum layerSizes
    compInfo      = ComponentCompiledInfo
                <$> pure (fromList $ zip layerReps layerCfgs)
                <*> pure compSize
                <*> prepareLayerInitializer      layerInfos
                <*> prepareLayersConstructor     layerInfos
                <*> prepareLayersDestructor      layerInfos
                <*> prepareSubComponentDiscovery layerOffInfos
                <*> MemPool.new def (MemPool.ItemSize compSize)

prepareLayerInitializer :: MonadIO m => [LayerInfo] -> m SomePtr
prepareLayerInitializer ls = do
    ptr <- mallocLayerInitializer ls
    liftIO $ fillLayerInitializer ptr ls
    pure ptr

mallocLayerInitializer :: MonadIO m => [LayerInfo] -> m SomePtr
mallocLayerInitializer = \case
    [] -> pure Ptr.nullPtr
    ls -> liftIO . Mem.mallocBytes . sum $ view layerByteSize <$> ls

fillLayerInitializer :: SomePtr -> [LayerInfo] -> IO ()
fillLayerInitializer = go where
    go ptr = \case
        []     -> pure ()
        (l:ls) -> mapM_ (flip (Mem.copyBytes ptr) byteSize) init
               >> go ptr' ls
            where init     = l ^. initializer
                  byteSize = l ^. layerByteSize
                  ptr'     = Ptr.plusPtr ptr byteSize

prepareLayersConstructor :: Monad m => [LayerInfo] -> m (SomePtr -> IO ())
prepareLayersDestructor  :: Monad m => [LayerInfo] -> m (SomePtr -> IO ())
prepareLayersConstructor = concatLayersIOActions constructor ; {-# INLINE prepareLayersConstructor #-}
prepareLayersDestructor  = concatLayersIOActions destructor  ; {-# INLINE prepareLayersDestructor #-}

-- | The function is defined in monad in order to compute the dynamic
--   initializer during monad resolution. Otherwise, the Maybe pattern
--   matching would be deffered to the layer initializer, which will
--   consequently run slower.
concatLayersIOActions :: Monad m
    => Lens' LayerInfo (Maybe (SomePtr -> IO ()))
    -> [LayerInfo]
    -> m (SomePtr -> IO ())
concatLayersIOActions lens = go where
    go = \case
        []           -> pure $ const (pure ())
        (!l : (!ls)) -> flip fuse (l ^. lens) =<< go ls where
            !byteSize = l ^. layerByteSize
            fuse !g   = \case
                Nothing -> pure $ \(!ptr) ->
                    let !out = g $! Ptr.plusPtr ptr byteSize
                    in  out
                Just f  -> pure $ \(!ptr) ->
                    let !proc1 = f ptr
                        !proc2 = g $! Ptr.plusPtr ptr byteSize
                        !out   = proc1 >> proc2
                    in  out
            {-# INLINE fuse #-}


-- FIXME : check if the above hack applies here too
prepareSubComponentDiscovery :: MonadIO m
    => [(Int, LayerInfo)] -> m (SomePtr -> IO [Component.Dynamic])
prepareSubComponentDiscovery ls = do
    pure $ (\ptr -> concat <$> mapM (uncurry $ getFromLayer ptr) ls)
    where
    getFromLayer :: SomePtr -> Int -> LayerInfo -> IO [Component.Dynamic]
    getFromLayer p off l = (l ^. subComponents) (p `Ptr.plusPtr` off)

