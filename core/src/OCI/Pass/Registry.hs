module OCI.Pass.Registry where

import Prologue as P

import qualified Control.Monad.State.Layered   as State
import qualified Data.Graph.Component.Class    as Component
import qualified Data.Graph.Component.Dynamic  as Component
import qualified Data.Graph.Component.Layer    as Layer
import qualified Data.Graph.Component.Provider as Component
import qualified Data.Map.Strict               as Map
import qualified Foreign.Storable1             as Storable1
import qualified Foreign.Storable1.Ptr         as Ptr1
import qualified OCI.Pass.State.IRInfo                 as Info

import Control.Monad.Exception     (Throws, throw)
import Control.Monad.State.Layered (StateT)
import Data.Graph.Component.Layer  (Layer)
import Data.Map.Strict             (Map)
import Foreign.Ptr.Utils           (SomePtr)
import Foreign.Storable1           (Storable1)
import OCI.Pass.State.IRInfo               (Info)


--------------------
-- === Errors === --
--------------------

data Error
    = MissingComponent Component.TagRep
    deriving (Show)

instance Exception Error



----------------------
-- === Registry === --
----------------------

-- === Definition === --

type Monad m = MonadRegistry m
type MonadRegistry m = (State.Monad Info m, Throws Error m, MonadIO m)

newtype RegistryT m a = RegistryT (StateT Info m a)
    deriving ( Applicative, Alternative, Functor, P.Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow)
makeLenses ''RegistryT


-- === Running === --

evalT :: Functor m => RegistryT m a -> m a
evalT = State.evalDefT . unwrap ; {-# INLINE evalT #-}

execT :: Functor m => RegistryT m a -> m Info
execT = State.execDefT . unwrap ; {-# INLINE execT #-}


-- === Component management === --

registerComponentRep :: MonadRegistry m => Component.TagRep -> m ()
registerComponentRep comp = State.modify_ @Info
                          $ Info.components %~ Map.insert comp def
{-# INLINE registerComponentRep #-}

registerComponent :: ∀ comp m. (MonadRegistry m, Typeable comp) => m ()
registerComponent = registerComponentRep (Component.tagRep @comp) ; {-# INLINE registerComponent #-}

registerPrimLayer :: ∀ comp layer m.
    ( MonadRegistry m
    , Typeable comp
    , Typeable layer
    , Layer    layer
    , Layer.StorableData  layer
    , Component.DynamicProvider1 (Layer.Cons layer)
    ) => m ()
registerPrimLayer = do
    let manager   = Layer.manager @layer
        ctor      = ctorDyn <$> manager ^. Layer.constructor
        dtor      = dtorDyn <$> manager ^. Layer.destructor
        size      = Layer.byteSize @layer
        comp      = Component.tagRep @comp
        layer     = someTypeRep @layer
    init <- mapM (fmap coerce . Ptr1.new) $ manager ^. Layer.initializer
    State.modifyM_ @Info $ \m -> do
        components' <- flip (at comp) (m ^. Info.components) $ \case
            Nothing       -> throw $ MissingComponent comp
            Just compInfo -> do
                pure $ Just $ compInfo & Info.layers %~ Map.insert layer
                    (Info.LayerInfo size init ctor dtor subComponentDyn)
        pure $ m & Info.components .~ components'
    where
    ctorDyn :: Storable1 t => IO (t a)       -> (SomePtr -> IO ())
    dtorDyn :: Storable1 t => (t a -> IO ()) -> (SomePtr -> IO ())
    ctorDyn t ptr = Storable1.poke (coerce ptr) =<< t ; {-# INLINE ctorDyn #-}
    dtorDyn f ptr = f =<< Storable1.peek (coerce ptr) ; {-# INLINE dtorDyn #-}

    subComponentDyn :: Component.DynamicProvider1 (Layer.Cons layer)
                    => SomePtr -> IO [Component.Dynamic]
    subComponentDyn ptr = Storable1.peek (coerce ptr)
                      >>= Component.dynamicComponents1 @(Layer.Cons layer)
{-# INLINE registerPrimLayer #-}



-- === Instances === --

instance P.Monad m => State.Getter Info (RegistryT m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance P.Monad m => State.Setter Info (RegistryT m) where
    put = wrap . State.put' ; {-# INLINE put #-}
