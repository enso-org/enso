{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeInType #-}

module OCI.Pass.Class where

import Prologue

import Control.Monad.Branch
import Control.Monad.State.Layered (StateT)
import Foreign (Ptr)

import OCI.IR.Selector
import OCI.IR.SelectorT

import qualified Control.Monad.Exception     as Exception
import qualified Control.Monad.State.Layered as State
import qualified Data.Map                    as Map
import qualified Data.Tuple.Strict           as Tuple
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Type.Data.List              as List
import qualified Foreign.Memory.Pool         as MemPool
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
    { _byteSize :: !Int
    , _layers   :: !(Map SomeTypeRep LayerConfig)
    , _memPool  :: !MemPool
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
newtype LayerByteOffset  comp layer = LayerByteOffset  Int
makeLenses ''ComponentMemPool
makeLenses ''LayerByteOffset


-- === Instances === --

instance Default (ComponentMemPool a)  where def = ComponentMemPool MemPool.unsafeNull ; {-# INLINE def #-}
instance Default (LayerByteOffset c l) where def = LayerByteOffset 0 ; {-# INLINE def #-}

instance (Typeable comp, Typeable layer) => Show (LayerByteOffset comp layer) where
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

newtype PassState       pass = PassState (PassStateData pass)
type    PassStateData   pass = TypeMap (PassStateLayout pass)
type    PassStateLayout pass = List.Append (ComponentMemPools pass)
                                           (LayersLayout      pass)

type ComponentMemPools pass = List.Map ComponentMemPool    (Vars pass Elems)
type LayersLayout      pass = MapComponentLayerLayout pass (Vars pass Elems)

type family MapComponentLayerLayout pass cs where
    MapComponentLayerLayout pass '[] = '[]
    MapComponentLayerLayout pass (c ': cs) = List.Append
        (ComponentLayerLayout pass c) (MapComponentLayerLayout pass cs)

type ComponentLayerLayout pass component
    = List.Map (LayerByteOffset component) (Vars pass component)

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

class    Monad m => DataGetter a   m    where getData :: m a
instance Monad m => DataGetter Imp m    where getData = impossible
instance            DataGetter a   ImpM where getData = impossible
instance (MonadPass m, TypeMap.ElemGetter a (DiscoverPassStateLayout m))
      => DataGetter a m where
    getData = TypeMap.getElem @a . unwrap <$> getPassState ; {-# INLINE getData #-}


type LayerByteOffsetGetter  comp layer m = DataGetter (LayerByteOffset  comp layer) m
type ComponentMemPoolGetter comp       m = DataGetter (ComponentMemPool comp)       m
getLayerByteOffset  :: ∀ comp layer m. LayerByteOffsetGetter  comp layer m => m Int
getComponentMemPool :: ∀ comp       m. ComponentMemPoolGetter comp       m => m MemPool
getLayerByteOffset  = unwrap <$> getData @(LayerByteOffset  comp layer) ; {-# INLINE getLayerByteOffset  #-}
getComponentMemPool = unwrap <$> getData @(ComponentMemPool comp)       ; {-# INLINE getComponentMemPool #-}


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

instance ( layers   ~ Vars      pass comp
         , targets  ~ ComponentLayerLayout pass comp
         , compMemPool ~ ComponentMemPool comp
         , Typeables layers
         , Typeable  comp
         , PassStateEncoder__  comps pass
         , PassDataElemEncoder  compMemPool MemPool pass
         , PassDataElemsEncoder targets Int pass
         ) => PassStateEncoder__ (comp ': comps) pass where
    passStateEncoder__ cfg = encoders where
        encoders   = appSemiLeft encoder subEncoder
        encoder    = procComp =<< mcomp
        subEncoder = passStateEncoder__ @comps cfg
        mcomp      = mapLeft (wrap . pure)
                   . lookupComponent tgtComp $ cfg ^. components
        tgtComp    = someTypeRep  @comp
        procComp i = (compEncoder .) <$> layerEncoder where
            compEncoder  = encodePassDataElem  @compMemPool (i ^. memPool)
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
    ([],rs)    -> Right rs
    ((l:ls),_) -> Left (l :| ls)
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
