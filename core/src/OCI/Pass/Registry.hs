module OCI.Pass.Registry where

import Prologue as P

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Foreign.Marshal.Utils       as Ptr
import qualified Foreign.Storable1           as Storable1
import qualified Foreign.Storable1.Ptr       as Ptr1
import qualified OCI.IR.Layer                as Layer

import Control.Monad.Exception     (Throws, throw)
import Control.Monad.State.Layered (StateT)
import Data.Map.Strict             (Map)
import Foreign.Ptr.Utils           (SomePtr)
import Foreign.Storable1           (Storable1)
import OCI.IR.Layer                (Layer)


-------------------
-- === State === --
-------------------

-- === Definition === --

newtype State = State
    { _components :: Map SomeTypeRep ComponentInfo
    } deriving (Default)

newtype ComponentInfo = ComponentInfo
    { _layers :: Map SomeTypeRep LayerInfo
    } deriving (Default, Mempty, Semigroup)

data LayerInfo = LayerInfo
    { _byteSize    :: !Int
    , _staticInit  :: !(Maybe SomePtr)
    , _dynamicInit :: !(Maybe (SomePtr -> IO ()))
    }


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



----------------------
-- === Registry === --
----------------------

-- === Definition === --

type Monad m = MonadRegistry m
type MonadRegistry m = (State.Monad State m, Throws Error m, MonadIO m)

newtype RegistryT m a = RegistryT (StateT State m a)
    deriving ( Applicative, Alternative, Functor, P.Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''RegistryT


-- === Running === --

evalT :: Functor m => RegistryT m a -> m a
evalT = State.evalDefT . unwrap ; {-# INLINE evalT #-}

execT :: Functor m => RegistryT m a -> m State
execT = State.execDefT . unwrap ; {-# INLINE execT #-}


-- === Component management === --

registerComponentRep :: MonadRegistry m => SomeTypeRep -> m ()
registerComponentRep comp = State.modifyM_ @State $ \m -> do
    when_ (Map.member comp $ m ^. components) . throw $ DuplicateComponent comp
    pure $ m & components %~ Map.insert comp def
{-# INLINE registerComponentRep #-}

registerPrimLayerRep :: MonadRegistry m
    => Int -> Maybe SomePtr -> Maybe (SomePtr -> IO ())
    -> SomeTypeRep -> SomeTypeRep -> m ()
registerPrimLayerRep s staticInit dynamicInit comp layer =
    State.modifyM_ @State $ \m -> do
        components' <- flip (at comp) (m ^. components) $ \case
            Nothing       -> throw $ MissingComponent comp
            Just compInfo -> do
                when_ (Map.member layer $ compInfo ^. layers) $
                    throw $ DuplicateLayer comp layer
                pure $ Just $ compInfo & layers %~ Map.insert layer
                    (LayerInfo s staticInit dynamicInit)
        pure $ m & components .~ components'
{-# INLINE registerPrimLayerRep #-}

registerComponent :: ∀ comp m.       (MonadRegistry m, Typeable comp) => m ()
registerPrimLayer :: ∀ comp layer m. (MonadRegistry m, Typeable comp, Typeable layer, Layer.StorableData layer, Layer layer) => m ()
registerComponent = registerComponentRep (someTypeRep @comp) ; {-# INLINE registerComponent #-}
registerPrimLayer = do
    initializer    <- liftIO $ fmap coerce . mapM Ptr1.new
                    $ Layer.initialize @layer
    let constructor = applyDyn <$> Layer.construct @layer
        byteSize    = Layer.byteSize @layer
        comp        = someTypeRep @comp
        layer       = someTypeRep @layer
    registerPrimLayerRep byteSize initializer constructor comp layer
    where applyDyn :: Storable1 t => IO (t a) -> (SomePtr -> IO ())
          applyDyn t ptr = Storable1.poke (coerce ptr) =<< t ; {-# INLINE applyDyn #-}
{-# INLINE registerPrimLayer #-}



-- === Instances === --

instance P.Monad m => State.Getter State (RegistryT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.Setter State (RegistryT m) where
    put = wrap . State.put' ; {-# INLINE put #-}
