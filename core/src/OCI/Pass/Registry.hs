module OCI.Pass.Registry where

import Prologue as P

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Foreign.Storable1           as Storable1
import qualified Foreign.Storable1.Ptr       as Ptr1
import qualified OCI.IR.Component.Provider   as Component
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
    { _byteSize      :: !Int
    , _initializer   :: !(Maybe SomePtr)
    , _constructor   :: !(Maybe (SomePtr -> IO ()))
    , _destructor    :: !(Maybe (SomePtr -> IO ()))
    , _DynamicGetter :: !(Component.DynamicGetter)
    }


-- === Instances === --

makeLenses ''State
makeLenses ''ComponentInfo
makeLenses ''LayerInfo



--------------------
-- === Errors === --
--------------------

data Error
    = MissingComponent SomeTypeRep
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
registerComponentRep comp = State.modify_ @State
                          $ components %~ Map.insert comp def
{-# INLINE registerComponentRep #-}

registerPrimLayerRep :: MonadRegistry m
    => Int                       -- ^ byte size
    -> Maybe SomePtr             -- ^ initial memory layout
    -> Maybe (SomePtr -> IO ())  -- ^ dynamic constructor
    -> Maybe (SomePtr -> IO ())  -- ^ dynamic destructor
    -> Component.DynamicGetter -- ^ func to retrieve all pointers of a comp.
    -> SomeTypeRep               -- ^ component type rep
    -> SomeTypeRep               -- ^ layer type rep
    -> m ()
registerPrimLayerRep size init ctor dtor ptrGetter comp layer =
    State.modifyM_ @State $ \m -> do
        components' <- flip (at comp) (m ^. components) $ \case
            Nothing       -> throw $ MissingComponent comp
            Just compInfo -> do
                pure $ Just $ compInfo & layers %~ Map.insert layer
                    (LayerInfo size init ctor dtor ptrGetter)
        pure $ m & components .~ components'
{-# INLINE registerPrimLayerRep #-}

registerComponent :: ∀ comp m. (MonadRegistry m, Typeable comp) => m ()
registerComponent = registerComponentRep (someTypeRep @comp) ; {-# INLINE registerComponent #-}

registerPrimLayer :: ∀ comp layer m.
    ( MonadRegistry m
    , Typeable comp
    , Typeable layer
    , Layer    layer
    , Layer.StorableData  layer
    , Component.Provider1 (Layer.Cons layer)
    ) => m ()
registerPrimLayer = do
    let manager   = Layer.manager @layer
        ctor      = ctorDyn <$> manager ^. Layer.constructor
        dtor      = dtorDyn <$> manager ^. Layer.destructor
        size      = Layer.byteSize @layer
        comp      = someTypeRep @comp
        layer     = someTypeRep @layer
        ptrGetter = Component.makeDynamicGetter $ Component.pointers1 @(Layer.Cons layer)
    init <- mapM (fmap coerce . Ptr1.new) $ manager ^. Layer.initializer
    registerPrimLayerRep size init ctor dtor ptrGetter comp layer
    where ctorDyn :: Storable1 t => IO (t a)       -> (SomePtr -> IO ())
          dtorDyn :: Storable1 t => (t a -> IO ()) -> (SomePtr -> IO ())
          ctorDyn t ptr = Storable1.poke (coerce ptr) =<< t ; {-# INLINE ctorDyn #-}
          dtorDyn f ptr = f =<< Storable1.peek (coerce ptr) ; {-# INLINE dtorDyn #-}
{-# INLINE registerPrimLayer #-}



-- === Instances === --

instance P.Monad m => State.Getter State (RegistryT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.Setter State (RegistryT m) where
    put = wrap . State.put' ; {-# INLINE put #-}
