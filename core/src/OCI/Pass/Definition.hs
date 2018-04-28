{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered  as State
import qualified Data.Graph.Component.Dynamic as Component
import qualified Data.Graph.Component.Layer   as Layer
import qualified Data.TypeMap.Strict          as TypeMap
import qualified Foreign.Memory.Pool          as MemPool
import qualified Foreign.Ptr                  as Ptr
import qualified OCI.Pass.Attr                as Attr
import qualified Type.Data.List               as List

import Control.Monad.State.Layered (StateT)
import Data.Graph.Component.Class  (Component)
import Data.TypeMap.Strict         (TypeMap)
import Foreign.Info.ByteSize       (ByteSize (ByteSize))
import Foreign.Memory.Pool         (MemPool)
import Foreign.Ptr.Utils           (SomePtr)
import GHC.Exts                    (Any)
import OCI.Pass.Attr               (Attr)



------------------------------
-- === Pass Declaration === --
------------------------------

-- | For example:
--
--   data MyPass
--   type instance Spec MyPass t = Spec_MyPass t
--   type family   Spec_MyPass t where
--       Spec_MyPass (In Elems) = '[Terms, Links]
--       Spec_MyPass (In Terms) = '[Model, Type]
--       Spec_MyPass (In Links) = '[Source, Target]
--       Spec_MyPass (Out a)    = Spec_MyPass (In a)
--       Spec_MyPass t          = '[]

-- === Definition === --

data Elems
data Attrs

data Property
    = PassIn        Type
    | PassOut       Type
    | PassPreserves Type


-- === Utils === --

type In        = 'PassIn
type Out       = 'PassOut
type Preserves = 'PassPreserves

type family Spec (pass :: Type) (prop :: Property) :: [Type]

type Ins  pass prop = Spec pass (In  prop)
type Outs pass prop = Spec pass (Out prop)
type Vars pass prop
    = List.Unique (List.Append (Ins pass prop) (Outs pass prop))



---------------------------
-- === Pass Metadata === --
---------------------------

-- === ComponentMemPool === --

newtype LayerByteOffset    comp layer = LayerByteOffset  Int
newtype ComponentTraversal comp       = ComponentTraversal
                                        (SomePtr -> IO [Component.Dynamic])

makeLenses ''LayerByteOffset
makeLenses ''ComponentTraversal


-- === Instances === --

instance Default (LayerByteOffset  c l) where def = wrap 0                   ; {-# INLINE def #-}
instance Default (ComponentTraversal c) where def = wrap (\_ -> pure mempty) ; {-# INLINE def #-}

instance (Typeable comp, Typeable layer)
      => Show (LayerByteOffset comp layer) where
    showsPrec d (unwrap -> a) = showParen' d $ showString name . showsPrec' a
        where name = (<> " ") $ unwords
                   [ "LayerByteOffset"
                   , '@' : show (typeRep @comp)
                   , '@' : show (typeRep @layer)
                   ]



------------------------
-- === Pass State === --
------------------------

-- === Definition === --

newtype     State       pass = State (StateData pass)
type        StateData   pass = TypeMap (StateLayout pass)
type family StateLayout pass :: [Type] -- CACHED WITH OCI.Pass.Cache.define
type ComputeStateLayout pass = List.Append (LayersLayout      pass)
                             ( List.Append (AttrValues        pass)
                             ( List.Append (ComponentMemPools pass)
                             ( List.Append (ComponentSizes    pass)
                             ( List.Append (DynamicGetters    pass)
                                           (LayerMemManagers pass )))))

type LayersLayout      pass = MapLayerByteOffset pass       (Vars pass Elems)
type ComponentMemPools pass = MapComponentMemPool           (Vars pass Elems)
type ComponentSizes    pass = MapComponentByteSize          (Vars pass Elems)
type DynamicGetters    pass = List.Map ComponentTraversal   (Vars pass Elems)
type LayerMemManagers  pass = List.Map Layer.DynamicManager (Vars pass Elems)
type AttrValues        pass = List.Map Attr                 (Vars pass Attrs)

type MapLayerByteOffset p c = MapOverCompsAndVars LayerByteOffset p c

type family MapComponentMemPool ls where
    MapComponentMemPool '[]       = '[]
    MapComponentMemPool (l ': ls) = MemPool (Component l ())
                                 ': MapComponentMemPool ls

type family MapComponentByteSize ls where
    MapComponentByteSize '[]       = '[]
    MapComponentByteSize (l ': ls) = ByteSize (Component l)
                                  ': MapComponentByteSize ls

type family MapOverCompsAndVars t pass comps where
    MapOverCompsAndVars t pass '[] = '[]
    MapOverCompsAndVars t pass (c ': cs) = List.Append
        (ComponentLayerLayout t pass c) (MapOverCompsAndVars t pass cs)

type ComponentLayerLayout t pass component
    = List.Map (t component) (Vars pass component)

makeLenses ''State


-- === Instances === --

deriving instance Show    (StateData pass) => Show    (State pass)
deriving instance Default (StateData pass) => Default (State pass)



------------------
-- === Pass === --
------------------

-- === Definition === --

newtype Pass (pass :: Type) a = Pass (StateT (State pass) IO a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow)
makeLenses ''Pass

class Definition pass where
    definition :: Pass pass ()


-- === Discovery === --

type family DiscoverPass m where
    DiscoverPass (Pass pass) = pass
    DiscoverPass (t m)       = DiscoverPass m

type DiscoverState       m = State       (DiscoverPass m)
type DiscoverStateData   m = StateData   (DiscoverPass m)
type DiscoverStateLayout m = StateLayout (DiscoverPass m)


-- === API === --

exec :: ∀ pass a. Pass pass a -> State pass -> IO (State pass)
exec !pass !state = State.execT (coerce pass) state ; {-# INLINE exec #-}

eval :: ∀ pass a. Pass pass a -> State pass -> IO ()
eval !pass !state = void $ exec pass state ; {-# INLINE eval #-}



-----------------
-- === Rep === --
-----------------

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep

rep :: ∀ (pass :: Type). Typeable pass => Rep
rep = wrap $ someTypeRep @pass ; {-# INLINE rep #-}

repOf :: ∀ pass a. Typeable pass => Pass pass a -> Rep
repOf _ = rep @pass ; {-# INLINE repOf #-}



-------------------------
--- === MonadState === --
-------------------------

-- === Definition === --

type MonadState m = State.Monad (DiscoverState m) m

instance State.Getter (State pass) (Pass pass) where
    get = wrap State.get' ; {-# INLINE get #-}

instance State.Setter (State pass) (Pass pass) where
    put = wrap . State.put' ; {-# INLINE put #-}


-- === API === --

class Monad m => DataGetter a m where getData :: m a
class Monad m => DataSetter a m where putData :: a -> m ()

instance Monad m => DataGetter Imp m     where getData = impossible
instance Monad m => DataSetter Imp m     where putData = impossible
instance            DataGetter a   ImpM1 where getData = impossible
instance            DataSetter a   ImpM1 where putData = impossible

instance (Monad m, MonadState m, TypeMap.ElemGetter a (DiscoverStateLayout m))
    => DataGetter a m where
    getData = TypeMap.getElem @a . unwrap <$> State.get @(DiscoverState m) ; {-# INLINE getData #-}

instance (Monad m, MonadState m, TypeMap.ElemSetter a (DiscoverStateLayout m))
    => DataSetter a m where
    putData a = State.modify_ @(DiscoverState m) $ wrapped %~ TypeMap.setElem a ; {-# INLINE putData #-}

type LayerByteOffsetGetter  c l m = DataGetter (LayerByteOffset  c l) m
type ComponentTraversalGetter c m = DataGetter (ComponentTraversal c) m
getLayerByteOffset    :: ∀ c l m. LayerByteOffsetGetter  c l m => m Int
getComponentTraversal :: ∀ c   m. ComponentTraversalGetter c m => m (ComponentTraversal c)
getLayerByteOffset     = unwrap <$> getData @(LayerByteOffset  c l) ; {-# INLINE getLayerByteOffset     #-}
getComponentTraversal  = getData @(ComponentTraversal c)            ; {-# INLINE getComponentTraversal  #-}


-- === Accessing pass data === --

instance {-# OVERLAPPABLE #-} DataGetter t (Pass pass)
    => State.Getter t (Pass pass) where
    get = getData @t ; {-# INLINE get #-}

instance {-# OVERLAPPABLE #-} DataSetter t (Pass pass)
    => State.Setter t (Pass pass) where
    put = putData @t ; {-# INLINE put #-}


-- === Layer Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData layer
    , LayerByteOffsetGetter comp layer (Pass pass)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Reader comp layer (Pass pass) where
    read__ !comp = do
        !off <- getLayerByteOffset @comp @layer
        Layer.unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData layer
    , LayerByteOffsetGetter comp layer (Pass pass)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Writer comp layer (Pass pass) where
    write__ !comp !d = do
        !off <- getLayerByteOffset @comp @layer
        Layer.unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}


-- === Layer.View Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableView layer layout
    , LayerByteOffsetGetter comp layer (Pass pass)
    ) => Layer.ViewReader comp layer layout (Pass pass) where
    readView__ !comp = do
        !off <- getLayerByteOffset @comp @layer
        Layer.unsafeReadViewByteOff @layer off comp
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableView layer layout
    , LayerByteOffsetGetter comp layer (Pass pass)
    ) => Layer.ViewWriter comp layer layout (Pass pass) where
    writeView__ !comp !d = do
        !off <- getLayerByteOffset @comp @layer
        Layer.unsafeWriteViewByteOff @layer off comp d
    {-# INLINE writeView__ #-}
