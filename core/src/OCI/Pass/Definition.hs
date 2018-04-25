{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified OCI.IR.Ptr.Provider         as PtrProvider
import qualified OCI.Pass.Attr               as Attr
import qualified Type.Data.List              as List

import Control.Monad.State.Layered (StateT)
import Data.TypeMap.Strict         (TypeMap)
import Foreign.Memory.Pool         (MemPool)
import Foreign.Ptr.Utils           (SomePtr)
import GHC.Exts                    (Any)


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

newtype AttrValue        attr       = AttrValue        Any
newtype ComponentMemPool comp       = ComponentMemPool MemPool
newtype ComponentSize    comp       = ComponentSize    Int
newtype LayerByteOffset  comp layer = LayerByteOffset  Int
data    LayerMemManager  comp       = LayerMemManager
    { _initializer :: SomePtr
    , _constructor :: SomePtr -> IO ()
    , _destructor  :: SomePtr -> IO ()
    }
newtype PointerGetter    comp = PointerGetter    PtrProvider.PointerGetter


makeLenses ''AttrValue
makeLenses ''ComponentMemPool
makeLenses ''ComponentSize
makeLenses ''LayerByteOffset
makeLenses ''PointerGetter
makeLenses ''LayerMemManager


-- === Instances === --

instance Default (AttrValue        a)   where def = wrap $ unsafeCoerce ()  ; {-# INLINE def #-}
instance Default (ComponentMemPool c)   where def = wrap MemPool.unsafeNull ; {-# INLINE def #-}
instance Default (ComponentSize    c)   where def = wrap 0                  ; {-# INLINE def #-}
instance Default (LayerByteOffset  c l) where def = wrap 0                  ; {-# INLINE def #-}
instance Default (LayerMemManager  c)   where
    def = LayerMemManager Ptr.nullPtr (const $ pure ()) (const $ pure ())
    {-# INLINE def #-}
instance Default (PointerGetter    c)   where def = wrap (\_ -> pure mempty) ; {-# INLINE def #-}

instance (Typeable comp, Typeable layer)
      => Show (LayerByteOffset comp layer) where
    showsPrec d (unwrap -> a) = showParen' d $ showString name . showsPrec' a
        where name = (<> " ") $ unwords
                   [ "LayerByteOffset"
                   , '@' : show (typeRep @comp)
                   , '@' : show (typeRep @layer)
                   ]

instance Typeable comp => Show (ComponentMemPool comp) where
    showsPrec d (unwrap -> a) = showParen' d $ showString name . showsPrec' a
        where name = (<> " ") $ unwords
                   [ "ComponentMemPool"
                   , '@' : show (typeRep @comp)
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
                             ( List.Append (PointerGetters    pass)
                                           (LayerMemManagers pass )))))

type LayersLayout      pass = MapLayerByteOffset        pass   (Vars pass Elems)
type ComponentMemPools pass = List.Map ComponentMemPool        (Vars pass Elems)
type ComponentSizes    pass = List.Map ComponentSize           (Vars pass Elems)
type PointerGetters    pass = List.Map PointerGetter           (Vars pass Elems)
type LayerMemManagers  pass = List.Map LayerMemManager         (Vars pass Elems)
type AttrValues        pass = List.Map AttrValue               (Vars pass Attrs)

type MapLayerByteOffset p c = MapOverCompsAndVars LayerByteOffset p c

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
type PointerGetterGetter    c   m = DataGetter (PointerGetter    c)   m
type LayerMemManagerGetter  c   m = DataGetter (LayerMemManager  c)   m
type ComponentMemPoolGetter c   m = DataGetter (ComponentMemPool c)   m
type ComponentSizeGetter    c   m = DataGetter (ComponentSize    c)   m
type AttrValueGetter        a   m = DataGetter (AttrValue        a)   m
type AttrValueSetter        a   m = DataSetter (AttrValue        a)   m
getLayerByteOffset     :: ∀ c l m. LayerByteOffsetGetter  c l m => m Int
getPointerGetter       :: ∀ c   m. PointerGetterGetter    c   m => m (PointerGetter    c)
getComponentMemPool    :: ∀ c   m. ComponentMemPoolGetter c   m => m MemPool
getComponentSize       :: ∀ c   m. ComponentSizeGetter    c   m => m Int
getLayerMemManager     :: ∀ c   m. LayerMemManagerGetter  c   m => m (LayerMemManager c)
getAttrValue           :: ∀ a   m. AttrValueGetter        a   m => m Any
putAttrValue           :: ∀ a   m. AttrValueSetter        a   m => Any -> m ()
getLayerByteOffset     = unwrap <$> getData @(LayerByteOffset  c l) ; {-# INLINE getLayerByteOffset     #-}
getComponentMemPool    = unwrap <$> getData @(ComponentMemPool c)   ; {-# INLINE getComponentMemPool    #-}
getComponentSize       = unwrap <$> getData @(ComponentSize    c)   ; {-# INLINE getComponentSize       #-}
getAttrValue           = unwrap <$> getData @(AttrValue        a)   ; {-# INLINE getAttrValue           #-}
getPointerGetter       = getData @(PointerGetter    c)              ; {-# INLINE getPointerGetter       #-}
getLayerMemManager     = getData @(LayerMemManager c)               ; {-# INLINE getLayerMemManager     #-}
putAttrValue           = putData @(AttrValue a) . wrap              ; {-# INLINE putAttrValue           #-}


-- === Instances === --

instance AttrValueGetter attr (Pass pass)
      => Attr.RawGetter attr (Pass pass) where
    getRaw = unsafeCoerce <$> getAttrValue @attr ; {-# INLINE getRaw #-}

instance AttrValueSetter attr (Pass pass)
      => Attr.RawSetter attr (Pass pass) where
    putRaw = putAttrValue @attr . unsafeCoerce ; {-# INLINE putRaw #-}
