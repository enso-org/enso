{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType                #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Class where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.Map                    as Map
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified Type.Data.List              as List

import Control.Monad.Exception     (Throws, throw)
import Control.Monad.State.Layered (StateT)
import Data.Map.Strict             (Map)
import Data.TypeMap.Strict         (TypeMap)
import Foreign.Memory.Pool         (MemPool)
import Foreign.Ptr.Utils           (SomePtr)



------------------------
-- === PassConfig === --
------------------------

-- === Definition === --

newtype PassConfig = PassConfig
    { _components :: Map SomeTypeRep ComponentConfig
    } deriving (Show)

data ComponentConfig = ComponentConfig
    { _byteSize  :: !Int
    , _layers    :: !(Map SomeTypeRep LayerConfig)
    , _layerInit :: !SomePtr
    , _memPool   :: !MemPool
    } deriving (Show)

newtype LayerConfig = LayerConfig
    { _byteOffset :: Int
    } deriving (Show)

makeLenses ''LayerConfig
makeLenses ''ComponentConfig
makeLenses ''PassConfig



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

compilePass :: State pass -> Pass pass a -> Compiled
compilePass !s p = Compiled . void $ flip State.evalT s (coerce p) ; {-# INLINE compilePass #-}

execPass :: State pass -> Pass pass a -> IO (State pass)
execPass !s p = flip State.execT s (coerce p) ; {-# INLINE execPass #-}


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



--------------------------------
-- === Pass State Encoder === --
--------------------------------

-- === Errors === --

data EncodingError
    = MissingComponent SomeTypeRep
    | MissingLayer     SomeTypeRep
    deriving (Show)

newtype EncoderError = EncoderError (NonEmpty EncodingError)
    deriving (Semigroup, Show)
makeLenses ''EncoderError

type EncoderResult  = Either EncoderError
type EncodingResult = Either EncodingError

instance Exception EncoderError


-- === API === --

tryEncodeState :: (StateEncoder pass, Default (State pass))
                   => PassConfig -> EncoderResult (State pass)
tryEncodeState cfg = ($ def) <$> passStateEncoder cfg ; {-# INLINE tryEncodeState #-}

encodeState ::
    ( StateEncoder pass
    , Default (State pass)
    , Throws EncoderError m
    ) => PassConfig -> m (State pass)
encodeState cfg = case tryEncodeState cfg of
    Left  e -> throw e
    Right a -> pure a
{-# INLINE encodeState #-}


-- === Encoding utils === --

passStateEncoder :: ∀ pass. StateEncoder pass
    => PassConfig -> EncoderResult (State pass -> State pass)
passStateEncoder = passStateEncoder__ @(Vars pass Elems) ; {-# INLINE passStateEncoder #-}

type  StateEncoder pass = StateEncoder__ (Vars pass Elems) pass
class StateEncoder__ (cs :: [Type]) pass where
    passStateEncoder__ :: PassConfig
                       -> EncoderResult (State pass -> State pass)

instance StateEncoder__ '[] pass where
    passStateEncoder__ _ = Right id ; {-# INLINE passStateEncoder__ #-}

instance ( layers   ~ Vars pass comp
         , targets  ~ ComponentLayerLayout LayerByteOffset pass comp
         , compMemPool ~ ComponentMemPool comp
         , compSize    ~ ComponentSize    comp
         , layerInit   ~ LayerInitializer comp
         , Typeables layers
         , Typeable  comp
         , StateEncoder__  comps pass
         , PassDataElemEncoder  compMemPool MemPool pass
         , PassDataElemEncoder  compSize    Int     pass
         , PassDataElemEncoder  layerInit   SomePtr pass
         , PassDataElemsEncoder targets     Int     pass
         ) => StateEncoder__ (comp ': comps) pass where
    passStateEncoder__ cfg = encoders where
        encoders   = appSemiLeft encoder subEncoder
        encoder    = procComp =<< mcomp
        subEncoder = passStateEncoder__ @comps cfg
        mcomp      = mapLeft (wrap . pure)
                   . lookupComponent tgtComp $ cfg ^. components
        tgtComp    = someTypeRep @comp
        procComp i = (encoders .) <$> layerEncoder where
            encoders     = initEncoder . memEncoder . sizeEncoder
            memEncoder   = encodePassDataElem  @compMemPool $ i ^. memPool
            initEncoder  = encodePassDataElem  @layerInit   $ i ^. layerInit
            sizeEncoder  = encodePassDataElem  @compSize    $ i ^. byteSize
            layerEncoder = encodePassDataElems @targets <$> layerOffsets
            layerTypes   = someTypeReps @layers
            layerOffsets = view byteOffset <<$>> layerInfos
            layerInfos   = mapLeft wrap $ catEithers
                         $ flip lookupLayer (i ^. layers) <$> layerTypes
    {-# INLINE passStateEncoder__ #-}

lookupComponent :: SomeTypeRep -> Map SomeTypeRep v -> EncodingResult v
lookupComponent k m = justErr (MissingComponent k) $ Map.lookup k m ; {-# INLINE lookupComponent #-}

lookupLayer :: SomeTypeRep -> Map SomeTypeRep v -> EncodingResult v
lookupLayer k m = justErr (MissingLayer k) $ Map.lookup k m ; {-# INLINE lookupLayer #-}

catEithers :: [Either l r] -> Either (NonEmpty l) [r]
catEithers lst = case partitionEithers lst of
    ([]    ,rs) -> Right rs
    ((l:ls),_)  -> Left $ l :| ls
{-# INLINE catEithers #-}

-- TODO
-- We should probably think about using other structure than Either here
-- Data.Validation is almost what we need, but its Semigroup instance
-- uses only lefts, which makes is not suitable for the purpose of
-- simple Either replacement.
appSemiLeft :: Semigroup e
            => (Either e (b -> c)) -> Either e (a -> b) -> Either e (a -> c)
appSemiLeft f a = case f of
    Left e -> case a of
        Left e' -> Left (e <> e')
        _       -> Left e
    Right ff -> case a of
        Left e   -> Left e
        Right aa -> Right $ ff . aa
{-# INLINE appSemiLeft #-}



-- === Element encoders === --

type PassDataElemEncoder   el t pass = PassDataElemsEncoder '[el] t pass
class PassDataElemsEncoder (els :: [Type]) t pass where
    encodePassDataElems :: [t] -> State pass -> State pass

encodePassDataElem :: ∀ el t pass. PassDataElemEncoder el t pass
                   => t -> State pass -> State pass
encodePassDataElem = encodePassDataElems @'[el] @t @pass . pure

instance PassDataElemsEncoder '[Imp] t   pass where encodePassDataElems = impossible
instance PassDataElemsEncoder els    Imp pass where encodePassDataElems = impossible
instance PassDataElemsEncoder els    t   Imp  where encodePassDataElems = impossible
instance TypeMap.SetElemsFromList els t (StateLayout pass)
      => PassDataElemsEncoder els t pass where
    encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals ; {-# INLINE encodePassDataElems #-}
