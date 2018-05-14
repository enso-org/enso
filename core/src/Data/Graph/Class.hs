{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.TypeMap.MultiState     as MultiState
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Type.Data.List              as List

import Data.TypeMap.MultiState (MultiStateT)



data Luna


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

-- === Encoder === --

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





type family Components      graph      :: [Type]
type family ComponentLayers graph comp :: [Type]

type State graph = TypeMap.TypeMap (StateData graph)


type StateData graph = MapLayerByteOffset graph (Components graph)


type MapLayerByteOffset graph comps
   = MapOverCompsAndLayers LayerByteOffset graph comps

type family MapOverCompsAndLayers f graph comps where
    MapOverCompsAndLayers f graph '[] = '[]
    MapOverCompsAndLayers f graph (c ': cs) = List.Append
        (MapOverLayers f graph c) (MapOverCompsAndLayers f graph cs)

type MapOverLayers f graph component
    = List.Map (f component) (ComponentLayers graph component)


type family DiscoverGraph (m :: Type -> Type) :: Type -- where
    -- DiscoverGraph (Graph tag m) = tag
    -- DiscoverGraph (t m)         = DiscoverGraph m

type DiscoverComponents      m = Components (DiscoverGraph m)
type DiscoverComponentLayers m comp = ComponentLayers (DiscoverGraph m) comp



---------------------
-- === Encoder === --
---------------------


-- === API === --

type EncoderResult = Either ()

type StateEncoder  graph m = (StateEncoder' graph, Applicative m)
type StateEncoder' graph   = TypeMap.Encoder (StateData graph)
                             () EncoderResult

tryEncodeState :: ∀ graph. StateEncoder' graph => EncoderResult (State graph)
tryEncodeState = let !out = TypeMap.encode () in out

encodeState :: ∀ graph m. (StateEncoder graph m, Monad m) => m (State graph)
encodeState = do
    !out <- case tryEncodeState @graph of
        Left  e -> error "UH!" -- throw e
        Right a -> pure a
    pure out
{-# NOINLINE encodeState #-}

-- === Instances === --




-------------------
-- === Graph === --
-------------------

-- === Definition === --

type    Graph  tag     = GraphT tag IO
newtype GraphT tag m a = GraphT (MultiStateT (StateData tag) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow, MonadTrans)
makeLenses ''GraphT


-- === API === --

run :: ∀ graph m a. (Monad m, StateEncoder graph m) => GraphT graph m a -> m a
run g = do
    !s <- encodeState @graph
    !out <- MultiState.evalT (unwrap g) s
    pure out
{-# INLINE run #-}

run2 :: ∀ graph m a. Monad m => GraphT graph m a -> TypeMap.TypeMap (StateData graph) -> m a
run2 !g !s = do
    !out <- MultiState.evalT (unwrap g) s
    pure out
{-# INLINE run2 #-}

-- run  :: ∀ tag m a. Monad m => GraphT tag m a -> State tag -> m (a, State tag)
-- exec :: ∀ tag m a. Monad m => GraphT tag m a -> State tag -> m (State tag)
-- eval :: ∀ tag m a. Monad m => GraphT tag m a -> State tag -> m a
-- run  = MultiState.runT  . unwrap ; {-# INLINE run  #-}
-- exec = MultiState.execT . unwrap ; {-# INLINE exec #-}
-- eval = MultiState.evalT . unwrap ; {-# INLINE eval #-}


-- === State === --

instance (Monad m, State.Getter a (MultiStateT (StateData tag) m))
      => State.Getter a (GraphT tag m) where
     get = wrap $ State.get @a ; {-# INLINE get #-}

instance (Monad m, State.Setter a (MultiStateT (StateData tag) m))
      => State.Setter a (GraphT tag m) where
     put = wrap . State.put @a ; {-# INLINE put #-}

