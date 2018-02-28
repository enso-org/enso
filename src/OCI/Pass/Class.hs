{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module OCI.Pass.Class where

import Prologue

import Control.Monad.Branch
import Control.Monad.State.Layered (StateT)
import Foreign (Ptr)

import OCI.IR.Selector
import OCI.IR.SelectorT

import Data.TypeMap.Strict (TypeMap)
import Data.Map.Strict     (Map)

import qualified Type.Data.List              as List
import qualified Data.Map                    as Map
import qualified Control.Monad.State.Layered as State
import qualified Data.Tuple.Strict           as Tuple
import qualified Data.TypeMap.Strict         as TypeMap

import qualified Control.Monad.Exception     as Exception








data CompilerException = ∀ e. Exception e => CompilerException e

instance Show CompilerException where
    show (CompilerException e) = show e

instance Exception CompilerException where
    displayException (CompilerException e) =
        "Luna compiler error.\n" <> displayException e <> "\n\n" <>
        "Please report it on https://github.com/luna/luna/issues"






------------------------
-- === PassConfig === --
------------------------

-- === Definition === --

data PassConfig = PassConfig
    { _components :: !(Map SomeTypeRep ComponentInfo)
    } deriving (Show)

data ComponentInfo = ComponentInfo
    { _byteSize :: !Int
    , _layers   :: !(Map SomeTypeRep LayerInfo)
    } deriving (Show)

data LayerInfo = LayerInfo
    { _byteOffset :: !Int
    } deriving (Show)

makeLenses ''LayerInfo
makeLenses ''ComponentInfo
makeLenses ''PassConfig



---------------------------------------
-- === Pass Layout configuration === --
---------------------------------------

-- === Definition === --

newtype ByteSize a = ByteSize Int deriving (Show)
newtype LayerByteOffset component layer = LayerByteOffset Int deriving (Show)
makeLenses ''ByteSize
makeLenses ''LayerByteOffset


-- === Instances === --

instance Default (ByteSize a)          where def = ByteSize        0 ; {-# INLINE def #-}
instance Default (LayerByteOffset c l) where def = LayerByteOffset 0 ; {-# INLINE def #-}



------------------------------
-- === Pass Declaration === --
------------------------------


-- | For example:
--   >> In MyPass (Terms / Layers) = '[Model]
type family Components pass :: [*]
type family In         pass component :: [*]
type family Out        pass component :: [*]
type family Preserves  pass component :: [*]



------------------------
-- === Pass State === --
------------------------

newtype PassState       pass = PassState (PassStateData pass)
type    PassStateData   pass = TypeMap (PassStateLayout pass)
type    PassStateLayout pass = List.Append (ComponentLayout pass)
                                           (LayersLayout    pass)

type ComponentLayout pass = List.Map ByteSize   (Components pass)
type LayersLayout    pass = LayersLayout__ pass (Components pass)

type family LayersLayout__ pass cs where
    LayersLayout__ pass '[] = '[]
    LayersLayout__ pass (c ': cs) = List.Append
        (ComponentLayerLayout pass c) (LayersLayout__ pass cs)

type ComponentLayerLayout pass component
    = List.Map (LayerByteOffset component) (ComponentLayers pass component)

type ComponentLayers pass component
    = (List.Append (In pass component) (Out pass component))

makeLenses ''PassState



-- === Instances === --

deriving instance Show    (PassStateData pass) => Show    (PassState pass)
deriving instance Default (PassStateData pass) => Default (PassState pass)



------------------
-- === Pass === --
------------------

-- === Definition === --

type       Pass pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT (PassState pass) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadTrans, MonadThrow, MonadBranch)
makeLenses ''SubPass


type family DiscoverPass m where
    DiscoverPass (SubPass pass m) = pass
    DiscoverPass (t m)            = DiscoverPass m

type DiscoverPassState m = PassState (DiscoverPass m)

class Monad m => MonadPass m where
    getPassState :: m (DiscoverPassState m)
    putPassState :: DiscoverPassState m -> m ()

instance Monad m => MonadPass (SubPass pass m) where
    getPassState = wrap State.get'   ; {-# INLINE getPassState #-}
    putPassState = wrap . State.put' ; {-# INLINE putPassState #-}

instance {-# OVERLAPPABLE #-}
         ( Monad (t m), MonadPass m, MonadTrans t
         , DiscoverPass (t m) ~ DiscoverPass m
         ) => MonadPass (t m) where
    getPassState = lift getPassState ; {-# INLINE getPassState #-}

runPass :: Functor m => PassState pass -> SubPass pass m a -> m a
runPass s = flip State.evalT s . coerce

{-# INLINE runPass #-}



--------------------------
-- === Pass Encoder === --
--------------------------

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


-- === API === --

encodePassStateTEMP :: (PassStateEncoder pass, Default (PassState pass)) => PassConfig -> PassState pass
encodePassStateTEMP cfg = case passStateEncoder cfg of
    Left e -> error (show e)
    Right f -> f def
{-# INLINE encodePassStateTEMP #-}

encodePassState :: (PassStateEncoder pass, Default (PassState pass))
                => PassConfig -> EncoderResult (PassState pass)
encodePassState cfg = ($ def) <$> passStateEncoder cfg ; {-# INLINE encodePassState #-}



-- === Encoding utils === --

passStateEncoder :: ∀ pass. PassStateEncoder pass
    => PassConfig -> EncoderResult (PassState pass -> PassState pass)
passStateEncoder = passStateEncoder__ @(Components pass) ; {-# INLINE passStateEncoder #-}

type  PassStateEncoder pass = PassStateEncoder__ (Components pass) pass
class PassStateEncoder__ (cs :: [Type]) pass where
    passStateEncoder__ :: PassConfig
                      -> EncoderResult (PassState pass -> PassState pass)

instance PassStateEncoder__ '[] pass where
    passStateEncoder__ _ = Right id ; {-# INLINE passStateEncoder__ #-}

instance ( layers   ~ ComponentLayers      pass comp
         , targets  ~ ComponentLayerLayout pass comp
         , compSize ~ ByteSize comp
         , Typeables layers
         , Typeable  comp
         , PassStateEncoder__  comps pass
         , EncodePassDataByteElems pass targets
         , EncodePassDataByteElem  pass (ByteSize comp)
         ) => PassStateEncoder__ (comp ': comps) pass where
    passStateEncoder__ cfg = encoders where
        encoders   = appSemiLeft encoder subEncoder
        encoder    = procComp =<< mcomp
        subEncoder = passStateEncoder__ @comps cfg
        mcomp      = mapLeft (wrap . pure)
                   . lookupComponent tgtComp $ cfg ^. components
        tgtComp    = someTypeRep  @comp
        procComp i = (compEncoder .) <$> layerEncoder where
            compEncoder  = encodePassDataElem  @compSize (i ^. byteSize)
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

-- TODO: We should probably think about using other structure than Either here
--       Data.Validation is almost what we need, but its Semigroup instance
--       uses only lefts, which makes is not suitable for the purpose of
--       simple Either replacement.
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

type EncodePassDataByteElem  pass el  = EncodePassDataByteElems pass '[el]
type EncodePassDataByteElems pass els = EncodePassDataElems Int pass els
type EncodePassDataElem    t pass el  = EncodePassDataElems t pass '[el]
type EncodePassDataElems   t pass els = TypeMap.SetElemsFromList els t
                                        (PassStateLayout pass)

encodePassDataElems :: ∀ (els :: [Type]) t pass. EncodePassDataElems t pass els
     => [t] -> PassState pass -> PassState pass
encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals ; {-# INLINE encodePassDataElems #-}

encodePassDataElem :: ∀ (el :: Type) t pass. EncodePassDataElem t pass el
    => t -> PassState pass -> PassState pass
encodePassDataElem = encodePassDataElems @'[el] . pure ; {-# INLINE encodePassDataElem #-}
