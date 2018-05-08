{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.State.Encoder where

import Prologue

import qualified Data.Graph.Component.Class      as Component
import qualified Data.Graph.Component.Dynamic    as Component
import qualified Data.Graph.Component.Layer      as Layer
import qualified Data.Graph.Component.Provider   as Component
import qualified Data.Map                        as Map
import qualified Data.TypeMap.Strict             as TypeMap
import qualified Foreign.Marshal.Alloc           as Mem
import qualified Foreign.Marshal.Utils           as Mem
import qualified Foreign.Memory.Pool             as MemPool
import qualified Foreign.Ptr                     as Ptr
import qualified OCI.Pass.Definition.Class       as Pass
import qualified OCI.Pass.Definition.Declaration as Pass
import qualified OCI.Pass.Management.Registry    as Reg
import qualified OCI.Pass.State.IRInfo           as IRInfo
import qualified OCI.Pass.State.Runtime          as Pass

import Control.Monad.Exception    (Throws, throw)
import Data.Graph.Component.Class (Component)
import Data.Map.Strict            (Map)
import Foreign.Info.ByteSize      (ByteSize (ByteSize))
import Foreign.Memory.Pool        (MemPool)
import Foreign.Ptr.Utils          (SomePtr)
import GHC.Exts                   (Any)
import OCI.Pass.State.Attr        (Attr)
import OCI.Pass.State.IRInfo      (CompiledIRInfo)



---------------------
-- === Encoder === --
---------------------

-- === Errors === --

data EncodingError
    = MissingComponent Component.TagRep
    | MissingLayer     SomeTypeRep
    deriving (Show)

newtype Error = Error (NonEmpty EncodingError)
    deriving (Semigroup, Show)
makeLenses ''Error

type EncoderResult  = Either Error
type EncodingResult = Either EncodingError

instance Exception Error


-- === API === --

type Encoding pass = (Encoder pass, Default (Pass.State pass))

tryRun :: Encoding pass => CompiledIRInfo -> EncoderResult (Pass.State pass)
tryRun cfg = ($ def) <$> encode cfg ; {-# INLINE tryRun #-}

run :: ∀ pass m. (Encoding pass, Throws Error m)
    => CompiledIRInfo -> m (Pass.State pass)
run cfg = case tryRun cfg of
    Left  e -> throw e
    Right a -> pure a
{-# INLINE run #-}


-- === Encoding utils === --

encode :: ∀ pass. Encoder pass
       => CompiledIRInfo -> EncoderResult (Pass.State pass -> Pass.State pass)
encode = encode__ @pass @(EncoderTarget pass) ; {-# INLINE encode #-}

type  Encoder       pass = Encoder__ pass (EncoderTarget pass)
type  EncoderTarget pass = Pass.Vars pass Pass.Elems
class Encoder__     pass (cs :: [Type]) where
    encode__ :: CompiledIRInfo -> EncoderResult (Pass.State pass -> Pass.State pass)

instance Encoder__ pass '[] where
    encode__ _ = Right id ; {-# INLINE encode__ #-}

instance ( layers      ~ Pass.Vars pass comp
         , targets     ~ Pass.ComponentLayerLayout Pass.LayerByteOffset pass comp
         , compMemPool ~ MemPool (Component comp ())
         , compSize    ~ ByteSize (Component comp)
         , compTravsl  ~ Component.DynamicTraversal comp
         , layerInit   ~ Layer.DynamicManager comp
         , TypeableMany layers
         , Typeable  comp
         , Encoder__ pass comps
         , PassDataElemEncoder  compMemPool pass
         , PassDataElemEncoder  compSize    pass
         , PassDataElemEncoder  compTravsl  pass
         , PassDataElemEncoder  layerInit   pass
         , PassDataElemsEncoder targets Int pass
         ) => Encoder__ pass (comp ': comps) where
    encode__ cfg = encode where
        encode     = appSemiLeft encoder subEncoder
        encoder    = procComp =<< mcomp
        subEncoder = encode__ @pass @comps cfg
        mcomp      = mapLeft (wrap . pure)
                   . lookupComp tgtComp $ cfg ^. IRInfo.compiledComponents
        tgtComp    = Component.tagRep @comp
        procComp i = (encoders .) <$> layerEncoder where
            memEncoder   = encodePassDataElem  @compMemPool
                         $ coerce (i ^. IRInfo.memPool)
            sizeEncoder  = encodePassDataElem  @compSize
                         $ ByteSize (i ^. IRInfo.layersByteSize)
            layerEncoder = encodePassDataElems @targets <$> layerOffsets
            initEncoder  = encodePassDataElem  @layerInit
                         $ Layer.DynamicManager @comp
                           (i ^. IRInfo.layersInitializer)
                           (i ^. IRInfo.layersConstructor)
                           (i ^. IRInfo.layersDestructor)
            travEncoder  = encodePassDataElem @compTravsl
                         . Component.DynamicTraversal @comp
                         $ i ^. IRInfo.layersComponents
            layerTypes   = someTypeReps @layers
            layerOffsets = view IRInfo.byteOffset <<$>> layerInfos
            layerInfos   = mapLeft wrap $ catEithers
                         $ flip lookupLayer (i ^. IRInfo.compiledLayers)
                       <$> layerTypes
            encoders     = travEncoder
                         . initEncoder
                         . memEncoder
                         . sizeEncoder

    {-# INLINE encode__ #-}

lookupComp :: Component.TagRep -> Map Component.TagRep v -> EncodingResult v
lookupComp k m = justErr (MissingComponent k) $ Map.lookup k m ; {-# INLINE lookupComp #-}

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


-- === Attr encoder === --

encodeAttrs :: ∀ pass. AttrEncoder pass
            => [Any] -> (Pass.State pass -> Pass.State pass)
encodeAttrs = encodeAttrs__ @pass @(AttrEncoderTarget pass) ; {-# INLINE encodeAttrs #-}

type  AttrEncoder       pass = AttrEncoder__ pass (AttrEncoderTarget pass)
type  AttrEncoderTarget pass = Pass.Vars pass Pass.Attrs
class AttrEncoder__     pass (attrs :: [Type]) where
    encodeAttrs__ :: [Any] -> (Pass.State pass -> Pass.State pass)

instance AttrEncoder__ pass '[] where
    encodeAttrs__ _ = id ; {-# INLINE encodeAttrs__ #-}

instance ( attr ~ Attr a
         , AttrEncoder__ pass as
         , PassDataElemEncoder attr pass
         , Typeable a
         ) => AttrEncoder__ pass (a ': as) where
    encodeAttrs__ []     = impossible
    encodeAttrs__ (a:as) = encoder . subEncoder where
        encoder    = encodePassDataElem @attr (unsafeCoerce a :: attr)
        subEncoder = encodeAttrs__ @pass @as as
    {-# INLINE encodeAttrs__ #-}


-- === Attr encoder === --

decodeAttrs    :: ∀ pass. AttrDecoder    pass => Pass.State pass -> [Any]
decodeOutAttrs :: ∀ pass. OutAttrDecoder pass => Pass.State pass -> [Any]
decodeAttrs    = decodeAttrs__ @pass @(AttrDecoderTarget    pass) ; {-# INLINE decodeAttrs #-}
decodeOutAttrs = decodeAttrs__ @pass @(OutAttrDecoderTarget pass) ; {-# INLINE decodeOutAttrs #-}

type  AttrDecoder          pass = AttrDecoder__ pass (AttrDecoderTarget    pass)
type  OutAttrDecoder       pass = AttrDecoder__ pass (OutAttrDecoderTarget pass)
type  AttrDecoderTarget    pass = Pass.Vars pass Pass.Attrs
type  OutAttrDecoderTarget pass = Pass.Outs pass Pass.Attrs
class AttrDecoder__        pass (attrs :: [Type]) where
    decodeAttrs__ :: Pass.State pass -> [Any]

instance AttrDecoder__ pass '[] where
    decodeAttrs__ _ = mempty ; {-# INLINE decodeAttrs__ #-}

instance ( attr ~ Attr a
         , AttrDecoder__ pass as
         , PassDataElemDecoder attr pass
         , Typeable a
         ) => AttrDecoder__ pass (a ': as) where
    decodeAttrs__ s = a : as where
        a  = unsafeCoerce $ decodePassDataElem @attr s
        as = decodeAttrs__ @pass @as s
    {-# INLINE decodeAttrs__ #-}


-- === Element encoders === --

type  PassDataElemEncoder  el pass = PassDataElemsEncoder '[el] el pass
class PassDataElemsEncoder (els :: [Type]) t pass where
    encodePassDataElems :: [t] -> Pass.State pass -> Pass.State pass

encodePassDataElem :: ∀ t pass. PassDataElemEncoder t pass
                   => t -> Pass.State pass -> Pass.State pass
encodePassDataElem = encodePassDataElems @'[t] @t @pass . pure

instance PassDataElemsEncoder '[Imp] t   pass where encodePassDataElems = imp
instance PassDataElemsEncoder els    Imp pass where encodePassDataElems = imp
instance PassDataElemsEncoder els    t   Imp  where encodePassDataElems = imp
instance TypeMap.SetElemsFromList els t (Pass.StateLayout pass)
      => PassDataElemsEncoder els t pass where
    encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals ; {-# INLINE encodePassDataElems #-}


type PassDataElemDecoder t pass = TypeMap.ElemGetter t (Pass.StateLayout pass)

decodePassDataElem :: ∀ t pass. PassDataElemDecoder t pass
                   => Pass.State pass -> t
decodePassDataElem = TypeMap.getElem @t . unwrap ; {-# INLINE decodePassDataElem #-}
