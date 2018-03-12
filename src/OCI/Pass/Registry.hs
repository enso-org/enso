module OCI.Pass.Registry where

import Prologue as P

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Foreign.Storable1.Ptr       as Ptr1
import qualified OCI.IR.Layer.Internal       as Layer

import Control.Monad.Exception     (Throws, throw)
import Control.Monad.State.Layered (MonadState, StateT)
import Data.Map.Strict             (Map)
import Foreign.Ptr.Utils           (SomePtr)


---------------------------
-- === Configuration === --
---------------------------

-- === Definition === --

newtype State = State
    { _components :: Map SomeTypeRep ComponentInfo
    } deriving (Default, Show)

newtype ComponentInfo = ComponentInfo
    { _layers :: Map SomeTypeRep LayerInfo
    } deriving (Default, Mempty, Semigroup, Show)

data LayerInfo = LayerInfo
    { _byteSize :: !Int
    , _defPtr   :: !SomePtr
    } deriving (Show)


-- === Instances === --

makeLenses ''State
makeLenses ''ComponentInfo
makeLenses ''LayerInfo



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

type Monad m = MonadRegistry m
type MonadRegistry m = (MonadState State m, Throws Error m, MonadIO m)

newtype RegistryT m a = RegistryT (StateT State m a)
    deriving ( Applicative, Alternative, Functor, P.Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''RegistryT


-- === Running === --

evalT :: Functor m => RegistryT m a -> m a
evalT = State.evalDefT . unwrap ; {-# INLINE evalT #-}


-- === Component management === --

registerComponentRep :: MonadRegistry m => SomeTypeRep -> m ()
registerComponentRep comp = State.modifyM_ @State $ \m -> do
    when_ (Map.member comp $ m ^. components) . throw $ DuplicateComponent comp
    pure $ m & components %~ Map.insert comp def
{-# INLINE registerComponentRep #-}

registerPrimLayerRep :: MonadRegistry m
                     => Int -> SomePtr -> SomeTypeRep -> SomeTypeRep -> m ()
registerPrimLayerRep s layerDef comp layer = State.modifyM_ @State $ \m -> do
    components' <- flip (at comp) (m ^. components) $ \case
        Nothing       -> throw $ MissingComponent comp
        Just compInfo -> do
            when_ (Map.member layer $ compInfo ^. layers) $
                throw $ DuplicateLayer comp layer
            pure $ Just $ compInfo & layers %~ Map.insert layer (LayerInfo s layerDef)
    pure $ m & components .~ components'
{-# INLINE registerPrimLayerRep #-}

registerComponent :: ∀ comp m.       (MonadRegistry m, Typeable comp) => m ()
registerPrimLayer :: ∀ comp layer m. (MonadRegistry m, Typeable comp, Typeable layer, Layer.StorableData comp layer, Layer.Initializer comp layer) => m ()
registerComponent = registerComponentRep (someTypeRep @comp) ; {-# INLINE registerComponent #-}
registerPrimLayer = do
    layerDef <- Ptr1.new $ Layer.init @comp @layer
    registerPrimLayerRep (Layer.byteSize @comp @layer) (coerce layerDef) (someTypeRep @comp) (someTypeRep @layer)
{-# INLINE registerPrimLayer #-}


-- === Instances === --

instance P.Monad m => State.MonadGetter State (RegistryT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.MonadSetter State (RegistryT m) where
    put = wrap . State.put' ; {-# INLINE put #-}
