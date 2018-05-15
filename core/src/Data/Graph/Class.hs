{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import           Prologue hiding (Monad)
import qualified Prologue as P

import qualified Control.Monad.State.Layered as State
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.TypeMap.MultiState     as MultiState
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Memory.Pool         as MemPool
import qualified Type.Data.List              as List

import Data.Graph.Data.Component.Class (Component)
import Data.TypeMap.MultiState         (MultiStateT)
import Foreign.Memory.Pool             (MemPool)
import Type.Data.List                  (type (<>))



data Luna



type family Components      graph      :: [Type]
type family ComponentLayers graph comp :: [Type]

type family DiscoverGraph (m :: Type -> Type) :: Type -- where
    -- DiscoverGraph (Graph graph m) = graph
    -- DiscoverGraph (t m)         = DiscoverGraph m

type DiscoverComponents      m = Components (DiscoverGraph m)
type DiscoverComponentLayers m comp = ComponentLayers (DiscoverGraph m) comp



-----------------------------
-- === LayerByteOffset === --
-----------------------------

-- === Definition === --

newtype LayerByteOffset comp layer = LayerByteOffset Int
makeLenses ''LayerByteOffset


-- === Instances === --

instance (Typeable comp, Typeable layer)
      => Show (LayerByteOffset comp layer) where
    showsPrec d (unwrap -> a) = showParen' d $ showString name . showsPrec' a
        where name = (<> " ") $ unwords
                   [ "LayerByteOffset"
                   , '@' : show (typeRep @comp)
                   , '@' : show (typeRep @layer)
                   ]




----------------------
-- === Encoders === --
----------------------

-- === LayerByteOffset === --

instance ( layers ~ DiscoverComponentLayers m comp
         , Applicative m
         , ComputeLayerByteOffset layer layers )
      => TypeMap.FieldEncoder (LayerByteOffset comp layer) () m where
    encodeField _ = pure $ LayerByteOffset $ computeLayerByteOffset @layer @layers

class ComputeLayerByteOffset layer (layers :: [Type]) where
    computeLayerByteOffset :: Int

instance ComputeLayerByteOffset l (l ': ls) where
    computeLayerByteOffset = 0 ; {-# INLINE computeLayerByteOffset #-}

instance {-# OVERLAPPABLE #-} (Layer.StorableData k, ComputeLayerByteOffset l ls)
      => ComputeLayerByteOffset l (k ': ls) where
    computeLayerByteOffset = Layer.byteSize @k + computeLayerByteOffset @l @ls
    {-# INLINE computeLayerByteOffset #-}


-- === MemPool === --

instance ( layers ~ DiscoverComponentLayers m comp
         , MonadIO m
         , ComputeComponentSize layers )
      => TypeMap.FieldEncoder (MemPool (Component comp layout)) () m where
    encodeField _ = MemPool.new def $ MemPool.ItemSize $ computeComponentSize @layers

class ComputeComponentSize (layers :: [Type]) where
    computeComponentSize :: Int

instance ComputeComponentSize '[] where
    computeComponentSize = 0 ; {-# INLINE computeComponentSize #-}

instance {-# OVERLAPPABLE #-} (Layer.StorableData l, ComputeComponentSize ls)
      => ComputeComponentSize (l ': ls) where
    computeComponentSize = Layer.byteSize @l + computeComponentSize @ls
    {-# INLINE computeComponentSize #-}



-------------------
-- === State === --
-------------------

newtype State      graph = State (StateData graph)
type    StateData  graph = TypeMap.TypeMap (StateElems graph)
type    StateElems graph = MapLayerByteOffset graph (Components graph)
                        <> MapComponentMemPool (Components graph)

type MapLayerByteOffset graph comps
   = MapOverCompsAndLayers LayerByteOffset graph comps

type family MapOverCompsAndLayers f graph comps where
    MapOverCompsAndLayers f graph '[] = '[]
    MapOverCompsAndLayers f graph (c ': cs) = List.Append
        (MapOverLayers f graph c) (MapOverCompsAndLayers f graph cs)

type MapOverLayers f graph component
    = List.Map (f component) (ComponentLayers graph component)

type family MapComponentMemPool ls where
    MapComponentMemPool '[]       = '[]
    MapComponentMemPool (l ': ls) = MemPool (Component l ())
                                 ': MapComponentMemPool ls

makeLenses ''State


















---------------------
-- === Encoder === --
---------------------


-- === API === --

type EncoderResult = Either ()

type StateEncoder graph m =
    ( MonadIO m
    , TypeMap.Encoder (StateElems graph) () m
    )

encodeState :: ∀ graph m. StateEncoder graph m => m (State graph)
encodeState = wrap <$> TypeMap.encode ()
{-# NOINLINE encodeState #-}

-- encodeState :: ∀ graph m. StateEncoder graph m => m (State graph)
-- encodeState = do
--     !out <- case tryEncodeState @graph of
--         Left  e -> error "UH!" -- throw e
--         Right a -> pure a
--     pure out
-- {-# NOINLINE encodeState #-}

-- === Instances === --




-------------------
-- === Graph === --
-------------------

-- === Definition === --

type    Graph  graph     = GraphT graph IO
newtype GraphT graph m a = GraphT (MultiStateT (StateElems graph) m a)
    deriving ( Applicative, Alternative, Functor, P.Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow, MonadTrans)
makeLenses ''GraphT


-- === API === --

run  :: ∀ graph m a. P.Monad m => GraphT graph m a -> State graph -> m (a, State graph)
exec :: ∀ graph m a. P.Monad m => GraphT graph m a -> State graph -> m (State graph)
eval :: ∀ graph m a. P.Monad m => GraphT graph m a -> State graph -> m a
run  g s = coerce <$> MultiState.runT  (unwrap g) (unwrap s) ; {-# INLINE run  #-}
exec g s = coerce <$> MultiState.execT (unwrap g) (unwrap s) ; {-# INLINE exec #-}
eval g s = MultiState.evalT (unwrap g) (unwrap s)            ; {-# INLINE eval #-}

encodeAndEval :: ∀ graph m a. StateEncoder graph m => GraphT graph m a -> m a
encodeAndEval g = eval g =<< encodeState ; {-# INLINE encodeAndEval #-}

getState :: ∀ graph m. Monad graph m => m (State graph)
getState = State.get @(State graph) ; {-# INLINE getState #-}


-- === State === --

type Monad graph m = State.Getter (State graph) m

instance {-# OVERLAPPABLE #-}
         (P.Monad m, State.Getter a (MultiStateT (StateElems graph) m))
      => State.Getter a (GraphT graph m) where
    get = wrap $ State.get @a ; {-# INLINE get #-}

instance {-# OVERLAPPABLE #-}
         (P.Monad m, State.Setter a (MultiStateT (StateElems graph) m))
      => State.Setter a (GraphT graph m) where
    put = wrap . State.put @a ; {-# INLINE put #-}

instance P.Monad m
      => State.Getter (State graph) (GraphT graph m) where
    get = wrap $! wrap <$> MultiState.getAll ; {-# INLINE get #-}
