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
import qualified OCI.Pass.State.Runtime          as Runtime
import qualified OCI.Pass.State.Runtime          as Pass

import Control.Monad.Exception    (Throws, throw)
import Data.Graph.Component.Class (Component, SomeComponent)
import Data.Map.Strict            (Map)
import Data.TypeMap.Strict        (TypeMap)
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
    | MissingLayer     Layer.Rep
    deriving (Show)

newtype Error = Error (NonEmpty EncodingError)
    deriving (Semigroup, Show)
makeLenses ''Error

type EncoderResult = Either Error

instance Exception Error



--------------------------
-- === StateEncoder === --
--------------------------

-- === Definition === --

type Encoder pass = StateEncoder (Pass.StateLayout pass)

class StateEncoder fields where
    encodeState :: CompiledIRInfo -> EncoderResult (TypeMap fields)


-- === API === --

tryRun :: ∀ pass. Encoder pass => CompiledIRInfo -> EncoderResult (Pass.State pass)
tryRun = fmap Runtime.State . encodeState ; {-# NOINLINE tryRun #-}

run :: ∀ pass m. (Encoder pass, Throws Error m)
    => CompiledIRInfo -> m (Pass.State pass)
run cfg = case tryRun cfg of
    Left  e -> throw e
    Right a -> pure a


-- === Instances === --

instance StateEncoder '[] where
    encodeState _ = pure TypeMap.empty

instance ( TypeMap.Prependable t ts
         , FieldEncoder t
         , StateEncoder ts
         ) => StateEncoder (t ': ts) where
    encodeState info = appSemiLeft (TypeMap.prepend <$> encodeField @t info)
                     $ encodeState @ts info



--------------------------
-- === FieldEncoder === --
--------------------------

-- === Definition === --

class FieldEncoder field where
    encodeField :: CompiledIRInfo -> EncoderResult field


-- === Instances === --

instance FieldEncoder CompiledIRInfo where
    encodeField = pure

instance Default (Attr a)
      => FieldEncoder (Attr a) where
    encodeField _ = pure def

instance Typeable comp
      => FieldEncoder (ByteSize (Component comp)) where
    encodeField info = do
        compInfo <- lookupComp  @comp  $ info ^. IRInfo.compiledComponents
        pure . wrap $ compInfo ^. IRInfo.layersByteSize

instance Typeable comp
      => FieldEncoder (Component.DynamicTraversal comp) where
    encodeField info = do
        compInfo <- lookupComp @comp $ info ^. IRInfo.compiledComponents
        pure . wrap $ compInfo ^. IRInfo.layersComponents

instance Typeable comp
      => FieldEncoder (MemPool (SomeComponent comp)) where
    encodeField info = do
        compInfo <- lookupComp @comp $ info ^. IRInfo.compiledComponents
        pure . coerce $ compInfo ^. IRInfo.memPool

instance Typeable comp
      => FieldEncoder (Layer.DynamicManager comp) where
    encodeField info = do
        compInfo <- lookupComp @comp $ info ^. IRInfo.compiledComponents
        pure $ Layer.DynamicManager
            (compInfo ^. IRInfo.layersInitializer)
            (compInfo ^. IRInfo.layersConstructor)
            (compInfo ^. IRInfo.layersDestructor)

instance Typeables '[comp,layer]
      => FieldEncoder (Pass.LayerByteOffset comp layer) where
    encodeField info = do
        compInfo  <- lookupComp  @comp  $ info ^. IRInfo.compiledComponents
        layerInfo <- lookupLayer @layer $ compInfo ^. IRInfo.compiledLayers
        pure . wrap $ layerInfo ^. IRInfo.byteOffset


-- === Helpers === --

lookupComp :: ∀ comp a. Typeable comp
           => Map Component.TagRep a -> EncoderResult a
lookupComp m = justErr (Error . pure $ MissingComponent k) $ Map.lookup k m
    where k = Component.tagRep @comp

lookupLayer :: ∀ layer a. Typeable layer
            => Map Layer.Rep a -> EncoderResult a
lookupLayer m = justErr (Error . pure $ MissingLayer k) $ Map.lookup k m
    where k = Layer.rep @layer

appSemiLeft :: Semigroup e
            => (Either e (a -> b)) -> Either e a -> Either e b
appSemiLeft f a = case f of
    Left e -> case a of
        Left e' -> Left (e <> e')
        _       -> Left e
    Right ff -> case a of
        Left e   -> Left e
        Right aa -> Right $ ff aa



-------------------------
-- === AttrEncoder === --
-------------------------

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
