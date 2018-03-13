{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Map.Strict             as Map
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified Type.Data.List              as List
import qualified Type.Known                  as Type

import Control.Monad.State.Layered (StateT)
import Data.Map.Strict             (Map)
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

newtype Attr             attr       = Attr             Any
newtype ComponentMemPool comp       = ComponentMemPool MemPool
newtype ComponentSize    comp       = ComponentSize    Int
newtype LayerInitializer comp       = LayerInitializer SomePtr
newtype LayerByteOffset  comp layer = LayerByteOffset  Int
makeLenses ''Attr
makeLenses ''ComponentMemPool
makeLenses ''ComponentSize
makeLenses ''LayerByteOffset
makeLenses ''LayerInitializer


-- === Instances === --

instance Default (Attr             a)   where def = wrap $ unsafeCoerce ()  ; {-# INLINE def #-}
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



---------------------
-- === Dynamic === --
---------------------

-- | Dynamic Passes are just like regular passes but with all the information
--   encoded in values instead of type level. Every pass is converted to its
--   dynamic form before execution. However, the encoded information is used
--   only by the pass manager to properly schedule the pass and update
--   attributes. It is not used to access components and layers. Such access
--   is optimized during compilation time to raw memory read and write.

-- === Definition === --

data Dynamic = Dynamic
    { _desc   :: Desc
    , _runner :: IO ()
    }

data Desc = Desc
    { _comps :: !(Map SomeTypeRep CompDesc)
    , _attrs :: !(Map SomeTypeRep AttrDesc)
    } deriving (Show)

data CompDesc = CompDesc
    { _io     :: !IODesc
    , _layers :: !(Map SomeTypeRep LayerDesc)
    } deriving (Show)

type LayerDesc = IODesc
type AttrDesc  = IODesc
data IODesc = IODesc
    { _isInput  :: !Bool
    , _isOutput :: !Bool
    } deriving (Show)


-- === Instances === --

instance Show Dynamic where
    show _ = "Pass.Dynamic" ; {-# INLINE show #-}

instance Default Desc where
    def = Desc def def ; {-# INLINE def #-}

instance Semigroup IODesc where
    IODesc i o <> IODesc i' o' = IODesc (i || i') (o || o') ; {-# INLINE (<>) #-}

makeLenses ''Dynamic
makeLenses ''Desc
makeLenses ''CompDesc
makeLenses ''IODesc


-- === API === --

uncheckedRunDynamic :: Dynamic -> IO ()
uncheckedRunDynamic = view runner ; {-# INLINE uncheckedRunDynamic #-}

compile :: ∀ pass a. Describe pass => State pass -> Pass pass a -> Dynamic
compile !s !p = Dynamic (describe @pass) . void $ flip State.evalT s (coerce p) ; {-# INLINE compile #-}

compile2 :: ∀ pass a. Describe pass => Pass pass a -> Any -> Dynamic
compile2 !p !s = compile (unsafeCoerce s) p ; {-# INLINE compile2 #-}

-- exec :: State pass -> Pass pass a -> IO (State pass)
-- exec !s p = flip State.execT s (coerce p) ; {-# INLINE exec #-}


class    Describe pass       where describe :: Desc
instance Describe Impossible where describe = impossible
instance {-# OVERLAPPABLE #-}
    ( comps ~ Vars pass Elems
    , DescAttrs pass
    , DescComps pass comps
    ) => Describe pass where
    describe = descAttrs @pass . descComps @pass @comps $ def ; {-# INLINE describe #-}

class    DescAttrs pass       where descAttrs :: Desc -> Desc
instance DescAttrs Impossible where descAttrs = impossible
instance ( inAttrs  ~ Ins  pass Attrs
         , outAttrs ~ Outs pass Attrs
         , Typeables inAttrs
         , Typeables outAttrs
         ) => DescAttrs pass where
    descAttrs = attrs .~ descIOMap ins outs where
        ins  = someTypeReps @inAttrs
        outs = someTypeReps @outAttrs
    {-# INLINE descAttrs #-}

class    DescComps pass (comps :: [Type]) where descComps :: Desc -> Desc
instance DescComps pass '[]               where descComps = id ; {-# INLINE descComps #-}
instance ( isIn      ~ List.In comp (Ins  pass Elems)
         , isOut     ~ List.In comp (Outs pass Elems)
         , inLayers  ~ Ins pass comp
         , outLayers ~ Ins pass comp
         , Type.Knowns '[isIn, isOut]
         , Typeable  comp
         , Typeables inLayers
         , Typeables outLayers
         , DescComps pass comps
         ) => DescComps pass (comp ': comps) where
    descComps = trans . descComps @pass @comps where
        trans     = comps %~ Map.insert compType compDesc
        compType  = someTypeRep @comp
        compDesc  = CompDesc compIO $ descIOMap inLayers outLayers
        compIO    = IODesc (Type.from @isIn) (Type.from @isOut)
        inLayers  = someTypeReps @inLayers
        outLayers = someTypeReps @outLayers

descIOMap :: [SomeTypeRep] -> [SomeTypeRep] -> Map SomeTypeRep LayerDesc
descIOMap ins outs = map where
    map   = flip (foldr ($)) updts
          $ Map.fromList . zip ins . repeat $ IODesc True False
    updts = flip (Map.insertWith (<>)) (IODesc False True) <$> outs



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
      => DataGetter a m where getData = TypeMap.getElem @a . unwrap <$> getState ; {-# INLINE getData #-}

type LayerByteOffsetGetter  c l m = DataGetter (LayerByteOffset  c l) m
type LayerInitializerGetter c   m = DataGetter (LayerInitializer c)   m
type ComponentMemPoolGetter c   m = DataGetter (ComponentMemPool c)   m
type ComponentSizeGetter    c   m = DataGetter (ComponentSize    c)   m
getLayerByteOffset  :: ∀ c l m. LayerByteOffsetGetter  c l m => m Int
getLayerInitializer :: ∀ c   m. LayerInitializerGetter c   m => m SomePtr
getComponentMemPool :: ∀ c   m. ComponentMemPoolGetter c   m => m MemPool
getComponentSize    :: ∀ c   m. ComponentSizeGetter    c   m => m Int
getLayerByteOffset  = unwrap <$> getData @(LayerByteOffset  c l) ; {-# INLINE getLayerByteOffset  #-}
getLayerInitializer = unwrap <$> getData @(LayerInitializer c)   ; {-# INLINE getLayerInitializer #-}
getComponentMemPool = unwrap <$> getData @(ComponentMemPool c)   ; {-# INLINE getComponentMemPool #-}
getComponentSize    = unwrap <$> getData @(ComponentSize    c)   ; {-# INLINE getComponentSize    #-}
