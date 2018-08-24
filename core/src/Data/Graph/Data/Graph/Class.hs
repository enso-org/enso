{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Graph.Class where

import           Prologue hiding (Monad)
import qualified Prologue as P

import qualified Control.Concurrent.Async    as Async
import qualified Control.Monad.State.Layered as State
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.TypeMap.MultiState     as MultiState
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Marshal.Alloc       as Mem
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified Foreign.Storable1           as Storable1
import qualified Type.Data.List              as List
import qualified Type.Known                  as Type

import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Layer.Class     (Layer)
import Data.TypeMap.MultiState         (MultiStateT)
import Foreign.Info.ByteSize           (ByteSize (ByteSize))
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import Foreign.Storable1               (Storable1)
import Type.Data.List                  (type (<>))





type family Components      graph      :: [Type]
type family ComponentLayers graph comp :: [Type]



type ComponentNumber graph = List.Length (Components graph)




-------------------
-- === State === --
-------------------

newtype State      graph = State (StateData graph)
type    StateData  graph = TypeMap.TypeMap (StateElems graph)
type    StateElems graph = -- MapLayerByteOffset graph      (Components graph)
                           MapComponentByteSize          (Components graph)
                        <> MapComponentMemPool           (Components graph)
                        <> List.Map Layer.DynamicManager (Components graph)

-- type MapLayerByteOffset graph comps
--    = MapOverCompsAndLayers Layer.ByteOffset graph comps

type family MapOverCompsAndLayers f graph comps where
    MapOverCompsAndLayers f graph '[] = '[]
    MapOverCompsAndLayers f graph (c ': cs) = List.Append
        (MapOverLayers f graph c) (MapOverCompsAndLayers f graph cs)

type MapOverLayers f graph comp
    = List.Map (f (Component comp)) (ComponentLayers graph comp)

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

type family   Discover (m :: Type -> Type) :: Type
type instance Discover (GraphT graph m)   = graph
type instance Discover (State.StateT s m) = Discover m

type ComponentsM      m      = Components      (Discover m)
type ComponentLayersM m comp = ComponentLayers (Discover m) comp

type ComponentNumberM m      = ComponentNumber (Discover m)

type KnownComponentNumber  g = Type.KnownInt (ComponentNumber  g)
type KnownComponentNumberM m = Type.KnownInt (ComponentNumberM m)


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



------------------------
-- === Concurrent === --
------------------------

async :: ∀ graph m a. (Monad graph m, MonadIO m)
      => GraphT graph IO a -> m (Async.Async a)
async m = liftIO . Async.async . eval m =<< getState
{-# INLINE async #-}

local :: ∀ graph m a. (Monad graph m, MonadIO m)
      => GraphT graph IO a -> m a
local m = liftIO . eval m =<< getState
{-# INLINE local #-}

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

-- === Layer.ByteOffset === --

-- instance ( layers ~ ComponentLayers graph comp
--          , Applicative m
--          , ComputeLayerByteOffset layer layers )
--       => FieldEncoder graph (Layer.ByteOffset (Component comp) layer) m where
--     encodeField = pure $ Layer.ByteOffset $ computeLayerByteOffset @layer @layers

instance
    ( layers ~ ComponentLayers graph comp
    , ComputeLayerByteOffset layer layers )
      => Layer.KnownLayer (Component comp) layer (Graph graph) where
    layerByteOffset = computeLayerByteOffset @layer @layers
    {-# INLINE layerByteOffset #-}

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
         , StorableLayers     layers
         , KnownComponentSize layers
         , MonadIO m )
      => FieldEncoder graph (Layer.DynamicManager comp) m where
    encodeField = do
        init <- liftIO . Mem.mallocBytes $ componentSize @layers
        liftIO $ mkCompInitializer @layers init
        pure $ Layer.DynamicManager
               init
            --    (mkCompConstructor @layers)
               (mkCompDestructor  @layers)

class MapLayersMem ctx (layers :: [Type]) where
    mapLayersMem :: (∀ layer a. ctx layer => Proxy layer
                                          -> Ptr.Ptr (Layer.Cons layer a)
                                          -> Maybe (IO ())
                    ) -> SomePtr -> IO ()

instance MapLayersMem ctx '[] where
    mapLayersMem _ _ = pure () ; {-# INLINE mapLayersMem #-}

instance {-# OVERLAPPABLE #-} (ctx l, Layer.StorableData l, MapLayersMem ctx ls)
      => MapLayersMem ctx (l ': ls) where
    mapLayersMem f ptr = out where
        size   = Layer.byteSize @l
        ptr'   = ptr `Ptr.plusPtr` size
        frest' = mapLayersMem @ctx @ls f ptr'
        out    = maybe id (>>) (f (Proxy :: Proxy l) (coerce ptr)) frest'
    {-# INLINABLE mapLayersMem #-}
    -- | INLINABLE is better for performance. Both INLINE and NOINLINE are
    --   worse. INLINE because of GHC bug regarding recursive functions, while
    --   NOINLINE because we got no specializations.

type StorableLayers layers = MapLayersMem StorableLayer layers
class    (Layer l, Layer.StorableData l) => StorableLayer l
instance (Layer l, Layer.StorableData l) => StorableLayer l

mkCompInitializer :: ∀ layers. StorableLayers layers => SomePtr -> IO ()
mkCompInitializer = mapLayersMem @StorableLayer @layers $ \(_ :: Proxy l) ptr ->
    (Storable1.poke ptr =<<) <$> Layer.manager @l ^. Layer.initializer
{-# INLINE mkCompInitializer #-}

-- mkCompConstructor :: ∀ layers. StorableLayers layers => SomePtr -> IO ()
-- mkCompConstructor = mapLayersMem @StorableLayer @layers $ \(_ :: Proxy l) ptr ->
    -- (Storable1.poke ptr =<<) <$> Layer.manager @l ^. Layer.constructor
-- {-# INLINE mkCompConstructor #-}

mkCompDestructor :: ∀ layers. StorableLayers layers => SomePtr -> IO ()
mkCompDestructor = mapLayersMem @StorableLayer @layers $ \(_ :: Proxy l) ptr ->
    (=<< Storable1.peek ptr) <$> Layer.manager @l ^. Layer.destructor
{-# INLINE mkCompDestructor #-}
