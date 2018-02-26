{-# LANGUAGE UndecidableInstances #-}

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

newtype ByteSize a = ByteSize Int deriving (Show)
makeLenses ''ByteSize

newtype LayerByteOffset component layer = LayerByteOffset Int deriving (Show)
makeLenses ''LayerByteOffset

-- data LayerInfo = LayerInfo
--     { _layerRep :: SomeTypeRep
--     , _layerByteOffset :: !Int
--     ,
--
-- }

panic :: a
panic = error "Internal error. Please report it as a bug at\
             \ https://github.com/luna/luna/issues"
{-# INLINE panic #-}



data LayerInfo = LayerInfo
    { _byteOffset :: !Int
    } deriving (Show)

data ComponentInfo = ComponentInfo
    { _byteSize :: !Int
    , _layers   :: !(Map SomeTypeRep LayerInfo)
    } deriving (Show)

data PassConfig = PassConfig
    { _components :: !(Map SomeTypeRep ComponentInfo)
    } deriving (Show)


makeLenses ''LayerInfo
makeLenses ''ComponentInfo
makeLenses ''PassConfig


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



type PassStateEncoder pass =
    ( ComponentLayoutEncoder pass
    , LayerLayoutEncoder     pass
    , Default (PassState pass)
    )

encodePassState :: PassStateEncoder pass => PassConfig -> PassState pass
encodePassState cfg = encodeLayersLayout     cfg
                    $ encodeComponentsLayout cfg
                    $ def
{-# INLINE encodePassState #-}

class ComponentLayoutEncoder pass where
    encodeComponentsLayout :: PassConfig -> PassState pass -> PassState pass

instance ( comps     ~ Components pass
         , compSizes ~ List.Map ByteSize comps
         , EncodePassDataByteElems pass compSizes
         , Typeables comps
         ) => ComponentLayoutEncoder pass where
    encodeComponentsLayout cfg = encodePassDataElems @compSizes compSizes where
        !comps     = someTypeReps @comps
        !compInfos = fromJust panic
                   $ sequence $ flip Map.lookup (cfg ^. components) <$> comps
        !compSizes = view byteSize <$> compInfos


type LayerLayoutEncoder pass = LayerLayoutEncoder__ (Components pass) pass
encodeLayersLayout :: ∀ pass. LayerLayoutEncoder pass
                   => PassConfig -> PassState pass -> PassState pass
encodeLayersLayout = encodeLayersLayout__ @(Components pass) ; {-# INLINE encodeLayersLayout #-}

class LayerLayoutEncoder__ (cs :: [Type]) pass where
    encodeLayersLayout__ :: PassConfig -> PassState pass -> PassState pass

instance LayerLayoutEncoder__ '[] pass where
    encodeLayersLayout__ _ = id ; {-# INLINE encodeLayersLayout__ #-}

instance ( layers  ~ ComponentLayers      pass comp
         , targets ~ ComponentLayerLayout pass comp
         , Typeables layers
         , Typeable  comp
         , LayerLayoutEncoder__  comps pass
         , EncodePassDataByteElems pass targets
         ) => LayerLayoutEncoder__ (comp ': comps) pass where
    encodeLayersLayout__ cfg = trans . encodeLayersLayout__ @comps cfg where
        !trans        = encodePassDataElems @targets layerOffsets
        !compInfo     = fromJust panic
                      $ Map.lookup (someTypeRep @comp) (cfg ^. components)
        !layerTypes   = someTypeReps @layers
        !layerInfos   = fromJust panic $ sequence
                      $ flip Map.lookup (compInfo ^. layers) <$> layerTypes
        !layerOffsets = view byteOffset <$> layerInfos
    {-# INLINE encodeLayersLayout__ #-}

-- instance EncodeLayerLayout '[] ls where
--     encodeLayerLayout _ = id ; {-# INLINE encodeLayerLayout #-}
--
-- instance EncodeLayerLayout (c ': cs) (l ': ls) where
--     encodeLayerLayout inf = where
--         Map.lookup (inf ^. )



-- class EncodeComponentByteOffsets pass t where
--     encodeComponentByteOffsets :: PassConfig -> t -> t

-- === API === --

type EncodePassDataByteElems pass els = EncodePassDataElems Int pass els
type EncodePassDataElems   t pass els = TypeMap.SetElemsFromList els t
                                        (PassStateLayout pass)

encodePassDataElems :: ∀ (els :: [Type]) t pass. EncodePassDataElems t pass els
     => [t] -> PassState pass -> PassState pass
encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals ; {-# INLINE encodePassDataElems #-}


-- === Instances === --

deriving instance Default (PassStateData pass) => Default (PassState pass)



------------------
-- === Pass === --
------------------

-- === Definition === --

type       Pass pass m   = SubPass pass m ()
newtype SubPass pass m a = SubPass (StateT (PassState pass) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadPlus, MonadTrans, MonadThrow, MonadBranch)
makeLenses ''SubPass


runPass :: Functor m => PassState pass -> SubPass pass m a -> m a
runPass s = flip State.evalT s . coerce
    -- termLayers = [someTypeRep @(TERM,Model)]

{-# INLINE runPass #-}
