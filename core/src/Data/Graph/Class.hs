{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import           Prologue hiding (Monad)
import qualified Prologue as P

import qualified Control.Monad.State.Layered as State
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.TypeMap.MultiState     as MultiState
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Marshal.Alloc       as Mem
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified Foreign.Storable1           as Storable1
import qualified Type.Data.List              as List

import Data.Graph.Data.Component.Class (Component)
import Data.TypeMap.MultiState         (MultiStateT)
import Foreign.Info.ByteSize           (ByteSize (ByteSize))
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import Type.Data.List                  (type (<>))





type family Components      graph      :: [Type]
type family ComponentLayers graph comp :: [Type]




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





-------------------
-- === State === --
-------------------

newtype State      graph = State (StateData graph)
type    StateData  graph = TypeMap.TypeMap (StateElems graph)
type    StateElems graph = MapLayerByteOffset graph      (Components graph)
                        <> MapComponentByteSize          (Components graph)
                        <> MapComponentMemPool           (Components graph)
                        <> List.Map Layer.DynamicManager (Components graph)

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

type family MapComponentByteSize ls where
    MapComponentByteSize '[]       = '[]
    MapComponentByteSize (l ': ls) = ByteSize (Component l)
                                  ': MapComponentByteSize ls

makeLenses ''State



-------------------
-- === Graph === --
-------------------

-- === Definition === --

type    Graph  graph     = GraphT graph IO
newtype GraphT graph m a = GraphT (MultiStateT (StateElems graph) m a)
    deriving ( Applicative, Alternative, Functor, P.Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow, MonadTrans)
makeLenses ''GraphT


-- === Discovery === --

type family DiscoverGraph (m :: Type -> Type) :: Type where
    DiscoverGraph (GraphT graph m) = graph
    DiscoverGraph (t m)            = DiscoverGraph m

type DiscoverComponents      m      = Components      (DiscoverGraph m)
type DiscoverComponentLayers m comp = ComponentLayers (DiscoverGraph m) comp


-- === API === --

run  :: ∀ graph m a. P.Monad m => GraphT graph m a -> State graph -> m (a, State graph)
exec :: ∀ graph m a. P.Monad m => GraphT graph m a -> State graph -> m (State graph)
eval :: ∀ graph m a. P.Monad m => GraphT graph m a -> State graph -> m a
run  g s = coerce <$> MultiState.runT  (unwrap g) (unwrap s) ; {-# INLINE run  #-}
exec g s = coerce <$> MultiState.execT (unwrap g) (unwrap s) ; {-# INLINE exec #-}
eval g s = MultiState.evalT (unwrap g) (unwrap s)            ; {-# INLINE eval #-}

encodeAndEval :: ∀ graph m a. Encoder graph m => GraphT graph m a -> m a
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





----------------------
-- === Encoders === --
----------------------

-- === Definition === --

class StateEncoder graph fields m where
    encode :: m (TypeMap.TypeMap fields)

class FieldEncoder graph field m where
    encodeField :: m field


-- === Instances === --

instance Applicative m
      => StateEncoder graph '[] m where
    encode = pure TypeMap.empty
    {-# INLINE encode #-}

instance ( Applicative m
         , StateEncoder graph ts m
         , FieldEncoder graph t  m
         , TypeMap.Prependable t ts )
      => StateEncoder graph (t ': ts) m where
    encode = TypeMap.prepend <$> encodeField @graph @t <*> encode @graph @ts
    {-# INLINE encode #-}

type Encoder graph m =
    ( P.Monad m
    , StateEncoder graph (StateElems graph) m
    )

encodeState :: ∀ graph m. Encoder graph m => m (State graph)
encodeState = wrap <$> encode @graph
{-# NOINLINE encodeState #-}



----------------------
-- === Encoders === --
----------------------

-- === LayerByteOffset === --

instance ( layers ~ ComponentLayers graph comp
         , Applicative m
         , ComputeLayerByteOffset layer layers )
      => FieldEncoder graph (LayerByteOffset comp layer) m where
    encodeField = pure $ LayerByteOffset $ computeLayerByteOffset @layer @layers

class ComputeLayerByteOffset layer (layers :: [Type]) where
    computeLayerByteOffset :: Int

instance ComputeLayerByteOffset l (l ': ls) where
    computeLayerByteOffset = 0 ; {-# INLINE computeLayerByteOffset #-}

instance {-# OVERLAPPABLE #-} (Layer.StorableData k, ComputeLayerByteOffset l ls)
      => ComputeLayerByteOffset l (k ': ls) where
    computeLayerByteOffset = Layer.byteSize @k + computeLayerByteOffset @l @ls
    {-# INLINE computeLayerByteOffset #-}


-- === Component ByteSize === --

instance ( layers ~ ComponentLayers graph comp
         , MonadIO m
         , KnownComponentSize layers )
      => FieldEncoder graph (ByteSize (Component comp)) m where
    encodeField = pure . wrap $ componentSize @layers

class KnownComponentSize (layers :: [Type]) where
    componentSize :: Int

instance KnownComponentSize '[] where
    componentSize = 0 ; {-# INLINE componentSize #-}

instance {-# OVERLAPPABLE #-} (Layer.StorableData l, KnownComponentSize ls)
      => KnownComponentSize (l ': ls) where
    componentSize = Layer.byteSize @l + componentSize @ls
    {-# INLINE componentSize #-}


-- === MemPool === --

instance ( layers ~ ComponentLayers graph comp
         , MonadIO m
         , KnownComponentSize layers )
      => FieldEncoder graph (MemPool (Component comp layout)) m where
    encodeField = MemPool.new def $ MemPool.ItemSize $ componentSize @layers


-- === Layer memory management === --


instance ( layers ~ ComponentLayers graph comp
         , ComputeComponentConstructor layers
         , ComputeComponentDestructor  layers
         , ComputeComponentStaticInit  layers
         , KnownComponentSize layers
         , MonadIO m )
      => FieldEncoder graph (Layer.DynamicManager comp) m where
    encodeField = do
        init <- liftIO . Mem.mallocBytes $ componentSize @layers
        liftIO $ computeComponentStaticInit @layers init
        pure $ Layer.DynamicManager
               init
               (computeComponentConstructor @layers)
               (computeComponentDestructor  @layers)

class ComputeComponentStaticInit (layers :: [Type]) where
    computeComponentStaticInit :: SomePtr -> IO ()

instance ComputeComponentStaticInit '[] where
    computeComponentStaticInit _ = pure () ; {-# INLINE computeComponentStaticInit #-}

instance {-# OVERLAPPABLE #-} (Layer.Layer l, Layer.StorableData l, ComputeComponentStaticInit ls)
      => ComputeComponentStaticInit (l ': ls) where
    computeComponentStaticInit ptr = out where
        mctor = Layer.manager @l ^. Layer.initializer
        size  = Layer.byteSize @l
        ptr'  = ptr `Ptr.plusPtr` size
        ctor' = computeComponentStaticInit @ls ptr'
        out    = maybe id (\f -> (dynf f >>)) mctor $ ctor'
        dynf f = Storable1.poke (coerce ptr) f
    {-# INLINE computeComponentStaticInit #-}



class ComputeComponentConstructor (layers :: [Type]) where
    computeComponentConstructor :: SomePtr -> IO ()

instance ComputeComponentConstructor '[] where
    computeComponentConstructor _ = pure () ; {-# INLINE computeComponentConstructor #-}

instance {-# OVERLAPPABLE #-} (Layer.Layer l, Layer.StorableData l, ComputeComponentConstructor ls)
      => ComputeComponentConstructor (l ': ls) where
    computeComponentConstructor ptr = out where
        mctor = Layer.manager @l ^. Layer.constructor
        size  = Layer.byteSize @l
        ptr'  = ptr `Ptr.plusPtr` size
        ctor' = computeComponentConstructor @ls ptr'
        out    = maybe id (\f -> (dynf f >>)) mctor $ ctor'
        dynf f = Storable1.poke (coerce ptr) =<< f
    {-# INLINE computeComponentConstructor #-}



class ComputeComponentDestructor (layers :: [Type]) where
    computeComponentDestructor :: SomePtr -> IO ()

instance ComputeComponentDestructor '[] where
    computeComponentDestructor _ = pure () ; {-# INLINE computeComponentDestructor #-}

instance {-# OVERLAPPABLE #-} (Layer.Layer l, Layer.StorableData l, ComputeComponentDestructor ls)
      => ComputeComponentDestructor (l ': ls) where
    computeComponentDestructor ptr = out where
        mctor = Layer.manager @l ^. Layer.destructor
        size  = Layer.byteSize @l
        ptr'  = ptr `Ptr.plusPtr` size
        ctor' = computeComponentDestructor @ls ptr'
        out    = maybe id (\f -> (dynf f >>)) mctor $ ctor'
        dynf f = f =<< Storable1.peek (coerce ptr)
    {-# INLINE computeComponentDestructor #-}



















