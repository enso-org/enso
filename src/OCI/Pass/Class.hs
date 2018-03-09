{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType #-}

module OCI.Pass.Class where

import Prologue

import Control.Monad.Branch
import Control.Monad.State.Layered (StateT)
import Foreign (Ptr)
import Foreign.Ptr.Utils (SomePtr)

import OCI.IR.Selector
import OCI.IR.SelectorT

import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified Data.Map                    as Map
import qualified Data.Tuple.Strict           as Tuple
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Type.Data.List              as List
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified OCI.IR.Layer.Internal       as Layer

import Data.TypeMap.Strict     (TypeMap)
import Data.Map.Strict         (Map)
import Foreign.Memory.Pool     (MemPool)
import Control.Monad.Exception (Throws, throw)



------------------------
-- === PassConfig === --
------------------------

-- === Definition === --

data PassConfig = PassConfig
    { _components :: !(Map SomeTypeRep ComponentConfig)
    } deriving (Show)

data ComponentConfig = ComponentConfig
    { _byteSize  :: !Int
    , _layers    :: !(Map SomeTypeRep LayerConfig)
    , _layerInit :: !SomePtr
    , _memPool   :: !MemPool
    } deriving (Show)

data LayerConfig = LayerConfig
    { _byteOffset :: !Int
    } deriving (Show)

makeLenses ''LayerConfig
makeLenses ''ComponentConfig
makeLenses ''PassConfig



------------------------------
-- === Pass Declaration === --
------------------------------


-- | For example: ...

data Elems

data Property
    = In        Type
    | Out       Type
    | Preserves Type

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

newtype     PassState       pass = PassState (PassStateData pass)
type        PassStateData   pass = TypeMap (PassStateLayout pass)
type family PassStateLayout pass :: [Type] -- CACHED WITH definePass TH
type ComputePassStateLayout pass = List.Append (ComponentMemPools pass)
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

makeLenses ''PassState


-- === Instances === --

deriving instance Show    (PassStateData pass) => Show    (PassState pass)
deriving instance Default (PassStateData pass) => Default (PassState pass)



------------------
-- === Pass === --
------------------

-- === Definition === --

type PassStdBase = IO

type    Pass     (pass :: Type)     = PassT    pass PassStdBase
type    SubPass  (pass :: Type)     = SubPassT pass PassStdBase
type    PassT    (pass :: Type) m   = SubPassT pass m ()
newtype SubPassT (pass :: Type) m a = SubPassT (StateT (PassState pass) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow, MonadBranch)
makeLenses ''SubPassT

type family DiscoverPass m where
    DiscoverPass (SubPassT pass m) = pass
    DiscoverPass (t m)            = DiscoverPass m

type DiscoverPassState       m = PassState       (DiscoverPass m)
type DiscoverPassStateData   m = PassStateData   (DiscoverPass m)
type DiscoverPassStateLayout m = PassStateLayout (DiscoverPass m)


-- === API === --

runPass :: Functor m => PassState pass -> SubPassT pass m a -> m a
runPass !s p = flip State.evalT s (coerce p) ; {-# INLINE runPass #-}



------------------------
--- === MonadPass === --
------------------------

-- === Definition === --

class Monad m => MonadPass m where
    getPassState :: m (DiscoverPassState m)
    putPassState :: DiscoverPassState m -> m ()

instance Monad m => MonadPass (SubPassT pass m) where
    getPassState = wrap State.get'   ; {-# INLINE getPassState #-}
    putPassState = wrap . State.put' ; {-# INLINE putPassState #-}

instance {-# OVERLAPPABLE #-}
         ( Monad (t m), MonadPass m, MonadTrans t
         , DiscoverPass (t m) ~ DiscoverPass m
         ) => MonadPass (t m) where
    getPassState = lift getPassState ; {-# INLINE getPassState #-}


-- === API === --

class    Monad m => DataGetter a   m     where getData :: m a
instance Monad m => DataGetter Imp m     where getData = impossible
instance            DataGetter a   ImpM1 where getData = impossible
instance (MonadPass m, TypeMap.ElemGetter a (DiscoverPassStateLayout m))
      => DataGetter a m where
    getData = TypeMap.getElem @a . unwrap <$> getPassState ; {-# INLINE getData #-}

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


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData comp layer
    , LayerByteOffsetGetter comp layer (SubPassT pass m)
    , MonadIO m
    ) => Layer.Reader comp layer (SubPassT pass m) where
    read__ !comp = do
        !off <- getLayerByteOffset @comp @layer
        Layer.unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData comp layer
    , LayerByteOffsetGetter comp layer (SubPassT pass m)
    , MonadIO m
    ) => Layer.Writer comp layer (SubPassT pass m) where
    write__ !comp !d = do
        !off <- getLayerByteOffset @comp @layer
        Layer.unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}



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

tryEncodePassState :: (PassStateEncoder pass, Default (PassState pass))
                   => PassConfig -> EncoderResult (PassState pass)
tryEncodePassState cfg = ($ def) <$> passStateEncoder cfg ; {-# INLINE tryEncodePassState #-}

encodePassState ::
    ( PassStateEncoder pass
    , Default (PassState pass)
    , Throws EncoderError m
    ) => PassConfig -> m (PassState pass)
encodePassState cfg = case tryEncodePassState cfg of
    Left  e -> throw e
    Right a -> return a
{-# INLINE encodePassState #-}


-- === Encoding utils === --

passStateEncoder :: ∀ pass. PassStateEncoder pass
    => PassConfig -> EncoderResult (PassState pass -> PassState pass)
passStateEncoder = passStateEncoder__ @(Vars pass Elems) ; {-# INLINE passStateEncoder #-}

type  PassStateEncoder pass = PassStateEncoder__ (Vars pass Elems) pass
class PassStateEncoder__ (cs :: [Type]) pass where
    passStateEncoder__ :: PassConfig
                       -> EncoderResult (PassState pass -> PassState pass)

instance PassStateEncoder__ '[] pass where
    passStateEncoder__ _ = Right id ; {-# INLINE passStateEncoder__ #-}

instance ( layers   ~ Vars pass comp
         , targets  ~ ComponentLayerLayout LayerByteOffset pass comp
         , compMemPool ~ ComponentMemPool comp
         , compSize    ~ ComponentSize    comp
         , layerInit   ~ LayerInitializer comp
         , Typeables layers
         , Typeable  comp
         , PassStateEncoder__  comps pass
         , PassDataElemEncoder  compMemPool MemPool pass
         , PassDataElemEncoder  compSize    Int     pass
         , PassDataElemEncoder  layerInit   SomePtr pass
         , PassDataElemsEncoder targets     Int     pass
         ) => PassStateEncoder__ (comp ': comps) pass where
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
        Left e -> Left e
        Right aa -> Right $ ff . aa
{-# INLINE appSemiLeft #-}



-- === Element encoders === --

type PassDataElemEncoder   el t pass = PassDataElemsEncoder '[el] t pass
class PassDataElemsEncoder (els :: [Type]) t pass where
    encodePassDataElems :: [t] -> PassState pass -> PassState pass

encodePassDataElem :: ∀ el t pass. PassDataElemEncoder el t pass
                   => t -> PassState pass -> PassState pass
encodePassDataElem = encodePassDataElems @'[el] @t @pass . pure

instance PassDataElemsEncoder '[Imp] t   pass where encodePassDataElems = impossible
instance PassDataElemsEncoder els    Imp pass where encodePassDataElems = impossible
instance PassDataElemsEncoder els    t   Imp  where encodePassDataElems = impossible
instance TypeMap.SetElemsFromList els t (PassStateLayout pass)
      => PassDataElemsEncoder els t pass where
    encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals ; {-# INLINE encodePassDataElems #-}
