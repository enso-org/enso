{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified Type.Data.List              as List

import Control.Monad.State.Layered (StateT)
import Data.TypeMap.Strict         (TypeMap)
import Foreign.Memory.Pool         (MemPool)
import Foreign.Ptr.Utils           (SomePtr)


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

data Elems
data Attrs

data Property
    = PassIn        Type
    | PassOut       Type
    | PassPreserves Type

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

newtype ComponentMemPool comp       = ComponentMemPool MemPool
newtype ComponentSize    comp       = ComponentSize    Int
newtype LayerInitializer comp       = LayerInitializer SomePtr
newtype LayerByteOffset  comp layer = LayerByteOffset  Int
makeLenses ''ComponentMemPool
makeLenses ''ComponentSize
makeLenses ''LayerByteOffset
makeLenses ''LayerInitializer


-- === Instances === --

instance Default (ComponentMemPool c)   where def = wrap MemPool.unsafeNull ; {-# INLINE def #-}
instance Default (ComponentSize    c)   where def = wrap 0                  ; {-# INLINE def #-}
instance Default (LayerInitializer c)   where def = wrap Ptr.nullPtr        ; {-# INLINE def #-}
instance Default (LayerByteOffset  c l) where def = wrap 0                  ; {-# INLINE def #-}

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
type ComputeStateLayout pass = List.Append (ComponentMemPools pass)
                             ( List.Append (ComponentSizes    pass)
                             ( List.Append (LayersLayout      pass)
                                           (LayerInitializers pass) ))

type ComponentMemPools pass = List.Map ComponentMemPool      (Vars pass Elems)
type ComponentSizes    pass = List.Map ComponentSize         (Vars pass Elems)
type LayerInitializers pass = List.Map LayerInitializer      (Vars pass Elems)
type LayersLayout      pass = MapLayerByteOffset        pass (Vars pass Elems)

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

type family DiscoverPass m where
    DiscoverPass (Pass pass) = pass
    DiscoverPass (t m)       = DiscoverPass m

type DiscoverState       m = State       (DiscoverPass m)
type DiscoverStateData   m = StateData   (DiscoverPass m)
type DiscoverStateLayout m = StateLayout (DiscoverPass m)


-- === Compiled Pass === --

newtype Compiled = Compiled { uncheckedRunCompiled :: IO () }


-- === API === --

compile :: State pass -> Pass pass a -> Compiled
compile !s p = Compiled . void $ flip State.evalT s (coerce p) ; {-# INLINE compile #-}

exec :: State pass -> Pass pass a -> IO (State pass)
exec !s p = flip State.execT s (coerce p) ; {-# INLINE exec #-}


-- === Instances === --

instance Show Compiled where show _ = "Pass.Compiled" ; {-# INLINE show #-}



-------------------------
--- === MonadState === --
-------------------------

-- === Definition === --

type  MonadStateCtx m = MonadIO m
class MonadStateCtx m => MonadState m where
    getState :: m (DiscoverState m)
    putState :: DiscoverState m -> m ()

instance MonadState (Pass pass) where
    getState = wrap State.get'   ; {-# INLINE getState #-}
    putState = wrap . State.put' ; {-# INLINE putState #-}

instance {-# OVERLAPPABLE #-}
         ( MonadStateCtx (t m), MonadState m, MonadTrans t
         , DiscoverPass (t m) ~ DiscoverPass m
         ) => MonadState (t m) where
    getState = lift   getState ; {-# INLINE getState #-}
    putState = lift . putState ; {-# INLINE putState #-}


-- === API === --

class    Monad m => DataGetter a   m     where getData :: m a
instance Monad m => DataGetter Imp m     where getData = impossible
instance            DataGetter a   ImpM1 where getData = impossible
instance (MonadState m, TypeMap.ElemGetter a (DiscoverStateLayout m))
      => DataGetter a m where
    getData = TypeMap.getElem @a . unwrap <$> getState ; {-# INLINE getData #-}

type LayerByteOffsetGetter  comp layer m = DataGetter (LayerByteOffset  comp layer) m
type LayerInitializerGetter comp       m = DataGetter (LayerInitializer comp)       m
type ComponentMemPoolGetter comp       m = DataGetter (ComponentMemPool comp)       m
type ComponentSizeGetter    comp       m = DataGetter (ComponentSize    comp)       m
getLayerByteOffset  :: ∀ comp layer m. LayerByteOffsetGetter  comp layer m => m Int
getLayerInitializer :: ∀ comp       m. LayerInitializerGetter comp       m => m SomePtr
getComponentMemPool :: ∀ comp       m. ComponentMemPoolGetter comp       m => m MemPool
getComponentSize    :: ∀ comp       m. ComponentSizeGetter    comp       m => m Int
getLayerByteOffset  = unwrap <$> getData @(LayerByteOffset  comp layer) ; {-# INLINE getLayerByteOffset  #-}
getLayerInitializer = unwrap <$> getData @(LayerInitializer comp)       ; {-# INLINE getLayerInitializer #-}
getComponentMemPool = unwrap <$> getData @(ComponentMemPool comp)       ; {-# INLINE getComponentMemPool #-}
getComponentSize    = unwrap <$> getData @(ComponentSize    comp)       ; {-# INLINE getComponentSize    #-}


