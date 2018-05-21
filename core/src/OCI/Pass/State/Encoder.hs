{-# LANGUAGE UndecidableInstances #-}

module OCI.Pass.State.Encoder where

import Prologue

import qualified Data.Graph.Component.Edge       as Edge
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Map                        as Map
import qualified Data.TypeMap.Strict             as TypeMap
import qualified Foreign.Marshal.Alloc           as Mem
import qualified Foreign.Marshal.Utils           as Mem
import qualified Foreign.Memory.Pool             as MemPool
import qualified Foreign.Ptr                     as Ptr
import qualified OCI.Pass.Definition.Class       as Pass
import qualified OCI.Pass.Definition.Declaration as Pass

import Control.Monad.Exception         (Throws, throw)
import Data.Graph.Data.Component.Class (Component)
import Data.Map.Strict                 (Map)
import Data.TypeMap.Strict             (TypeMap)
import Foreign.Info.ByteSize           (ByteSize (ByteSize))
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import GHC.Exts                        (Any)
import OCI.IR.Term                     (Terms)
import OCI.Pass.State.Attr             (Attr)



-------------------------
-- === AttrEncoder === --
-------------------------

encodeAttrs :: ∀ pass. AttrEncoder pass
            => [Any] -> (Pass.State pass -> Pass.State pass)
encodeAttrs = encodeAttrs__ @pass @(AttrEncoderTarget pass)

type  AttrEncoder       pass = AttrEncoder__ pass (AttrEncoderTarget pass)
type  AttrEncoderTarget pass = Pass.Vars pass Pass.Attrs
class AttrEncoder__     pass (attrs :: [Type]) where
    encodeAttrs__ :: [Any] -> (Pass.State pass -> Pass.State pass)

instance AttrEncoder__ pass '[] where
    encodeAttrs__ _ = id

instance ( attr ~ Attr a
         , AttrEncoder__ pass as
         , PassDataElemEncoder attr pass
         , Typeable a
         ) => AttrEncoder__ pass (a ': as) where
    encodeAttrs__ []     = impossible
    encodeAttrs__ (a:as) = encoder . subEncoder where
        encoder    = encodePassDataElem @attr (unsafeCoerce a :: attr)
        subEncoder = encodeAttrs__ @pass @as as


-- === Attr encoder === --

decodeAttrs    :: ∀ pass. AttrDecoder    pass => Pass.State pass -> [Any]
decodeOutAttrs :: ∀ pass. OutAttrDecoder pass => Pass.State pass -> [Any]
decodeAttrs    = decodeAttrs__ @pass @(AttrDecoderTarget    pass)
decodeOutAttrs = decodeAttrs__ @pass @(OutAttrDecoderTarget pass)

type  AttrDecoder          pass = AttrDecoder__ pass (AttrDecoderTarget    pass)
type  OutAttrDecoder       pass = AttrDecoder__ pass (OutAttrDecoderTarget pass)
type  AttrDecoderTarget    pass = Pass.Vars pass Pass.Attrs
type  OutAttrDecoderTarget pass = Pass.Outs pass Pass.Attrs
class AttrDecoder__        pass (attrs :: [Type]) where
    decodeAttrs__ :: Pass.State pass -> [Any]

instance AttrDecoder__ pass '[] where
    decodeAttrs__ _ = mempty

instance ( attr ~ Attr a
         , AttrDecoder__ pass as
         , PassDataElemDecoder attr pass
         , Typeable a
         ) => AttrDecoder__ pass (a ': as) where
    decodeAttrs__ s = a : as where
        a  = unsafeCoerce $ decodePassDataElem @attr s
        as = decodeAttrs__ @pass @as s


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
    encodePassDataElems vals = wrapped %~ TypeMap.setElemsFromList @els vals


type PassDataElemDecoder t pass = TypeMap.ElemGetter t (Pass.StateLayout pass)

decodePassDataElem :: ∀ t pass. PassDataElemDecoder t pass
                   => Pass.State pass -> t
decodePassDataElem = TypeMap.getElem @t . unwrap
