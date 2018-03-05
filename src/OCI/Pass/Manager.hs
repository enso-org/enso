module OCI.Pass.Manager where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Foreign.Storable.Utils      as Storable
import qualified OCI.IR.Layer                as Layer
import qualified OCI.Pass.Class              as Pass
import qualified Foreign.Memory.Pool         as MemPool

import Control.Lens                (at)
import Control.Monad.Exception     (Throws, throw)
import Control.Monad.State.Layered (MonadState, StateT)
import Data.Map.Strict             (Map)
import Data.Set                    (Set)
import Foreign.Memory.Pool         (MemPool)
import OCI.IR.Layer                (Layer)


---------------------------
-- === Configuration === --
---------------------------

-- === Definition === --

newtype Registry = Registry
    { _components :: Map SomeTypeRep ComponentInfo
    } deriving (Default, Mempty, Semigroup, Show)

newtype ComponentInfo = ComponentInfo
    { _layers :: Map SomeTypeRep LayerInfo
    } deriving (Default, Mempty, Semigroup, Show)

newtype LayerInfo = LayerInfo
    { _byteSize :: Int
    } deriving (Show)

makeLenses ''Registry
makeLenses ''ComponentInfo
makeLenses ''LayerInfo


-- === Instances === --



-- === Pass config preparation === --

mkPassConfig :: MonadIO m => Registry -> m Pass.PassConfig
mkPassConfig cfg = Pass.PassConfig <$> mapM mkCompConfig (cfg ^. components) ; {-# INLINE mkPassConfig #-}

mkCompConfig :: MonadIO m => ComponentInfo -> m Pass.ComponentConfig
mkCompConfig compCfg = compInfo where
    layerReps    = Map.keys  $ compCfg ^. layers
    layerInfos   = Map.elems $ compCfg ^. layers
    layerSizes   = view byteSize <$> layerInfos
    layerOffsets = scanl (+) 0 layerSizes
    layerCfgs    = Pass.LayerConfig <$> layerOffsets
    compSize     = sum layerSizes
    compInfo     = Pass.ComponentConfig compSize
                   (fromList $ zip layerReps layerCfgs)
               <$> MemPool.new compSize
{-# INLINE mkCompConfig #-}


-- data Config = Config
--     { _components :: !(Map SomeTypeRep ComponentInfo)
--     } deriving (Show)
--
-- data ComponentInfo = ComponentInfo
--     { _byteSize :: !Int
--     , _layers   :: !(Map SomeTypeRep LayerInfo)
--     , _memPool  :: !MemPool
--     } deriving (Show)
--
-- data LayerInfo = LayerInfo
--     { _byteOffset :: !Int
--     } deriving (Show)


--------------------
-- === Errors === --
--------------------


data Error
    = DuplicateComponent SomeTypeRep
    | DuplicateLayer     SomeTypeRep SomeTypeRep
    | MissingComponent   SomeTypeRep
    deriving (Show)

instance Exception Error



-------------------------
-- === PassManager === --
-------------------------


-- === Definition === --

-- data State = State
--     { _components :: Map SomeTypeRep ComponentInfo
--     -- , _primLayers :: Map SomeTypeRep LayerInfo
--     } deriving (Show)
-- makeLenses ''State

type MonadPassManager m = (MonadState Registry m, Throws Error m)

newtype PassManagerT m a = PassManagerT (StateT Registry m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''PassManagerT



-- === Running === --

evalT :: Functor m => PassManagerT m a -> m a
evalT = State.evalDefT . unwrap ; {-# INLINE evalT #-}



-- === Component management === --

registerComponentRep :: MonadPassManager m => SomeTypeRep -> m ()
registerComponentRep comp = State.modifyM_ @Registry $ \m -> do
    when_ (Map.member comp $ m ^. components) . throw $ DuplicateComponent comp
    return $ m & components %~ Map.insert comp def
{-# INLINE registerComponentRep #-}

registerPrimLayerRep :: MonadPassManager m => Int -> SomeTypeRep -> SomeTypeRep -> m ()
registerPrimLayerRep s comp layer = State.modifyM_ @Registry $ \m -> do
    components' <- flip (at comp) (m ^. components) $ \case
        Nothing       -> throw $ MissingComponent comp
        Just compInfo -> do
            when_ (Map.member layer $ compInfo ^. layers) $
                throw $ DuplicateLayer comp layer
            return $ Just $ compInfo & layers %~ Map.insert layer (LayerInfo s)
    return $ m & components .~ components'
{-# INLINE registerPrimLayerRep #-}

registerComponent :: ∀ comp m.       (MonadPassManager m, Typeable comp) => m ()
registerPrimLayer :: ∀ comp layer m. (MonadPassManager m, Typeable comp, Typeable layer, Layer comp layer) => m ()
registerComponent = registerComponentRep (someTypeRep @comp) ; {-# INLINE registerComponent #-}
registerPrimLayer = registerPrimLayerRep (Layer.byteSize @comp @layer) (someTypeRep @comp) (someTypeRep @layer) ; {-# INLINE registerPrimLayer #-}


-- === Instances === --

instance Monad m => State.MonadGetter Registry (PassManagerT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance Monad m => State.MonadSetter Registry (PassManagerT m) where
    put = wrap . State.put' ; {-# INLINE put #-}


test :: PassManagerT IO ()
test = do
    x <- State.get @Registry
    return ()
