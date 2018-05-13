{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.TypeMap.Strict         as TypeMap
import qualified OCI.Pass.State.Runtime      as MultiState
import qualified Type.Data.List              as List

import OCI.Pass.State.Runtime (MultiStateT)



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
tryEncodeState = TypeMap.encode ()

encodeState :: ∀ graph m. StateEncoder graph m => m (State graph)
encodeState = case tryEncodeState @graph of
    -- Left  e -> throw e
    Right a -> pure a


-- === Instances === --

instance ( layers ~ DiscoverComponentLayers m comp
         , Applicative m
         , LayerByteOffsetEncoder layer layers )
      => TypeMap.FieldEncoder (LayerByteOffset comp layer) () m where
    encodeField _ = pure $ LayerByteOffset $ encodeLayerByteOffset @layer @layers

class LayerByteOffsetEncoder layer (layers :: [Type]) where
    encodeLayerByteOffset :: Int

instance LayerByteOffsetEncoder l (l ': ls) where
    encodeLayerByteOffset = 0 ; {-# INLINE encodeLayerByteOffset #-}

instance {-# OVERLAPPABLE #-} (Layer.StorableData k, LayerByteOffsetEncoder l ls)
      => LayerByteOffsetEncoder l (k ': ls) where
    encodeLayerByteOffset = Layer.byteSize @k + encodeLayerByteOffset @l @ls ; {-# INLINE encodeLayerByteOffset #-}



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
run g = MultiState.evalT (unwrap g) =<< encodeState @graph ; {-# INLINE run #-}

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

