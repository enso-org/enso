{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.State.Runtime where

import Prologue

import qualified Control.Monad.State.Layered  as State
import qualified Data.Graph.Component         as Component
import qualified Data.Graph.Component.Dynamic as Component
import qualified Data.Graph.Component.Layer   as Layer
import qualified Data.TypeMap.Strict          as TypeMap
import qualified OCI.Pass.State.IRInfo        as IRInfo
import qualified Type.Data.List               as List

import Control.Monad.State.Layered     (StateT)
import Data.Graph.Component.Class      (Component)
import Data.TypeMap.Strict             (TypeMap)
import Foreign.Info.ByteSize           (ByteSize)
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import OCI.Pass.Definition.Declaration (Attrs, Elems, Vars)
import OCI.Pass.State.Attr             (Attr)
import OCI.Pass.State.IRInfo           (CompiledIRInfo)
import Type.Data.List                  (type (<>))



----------------------
-- === Metadata === --
----------------------

-- === Definition === --

newtype LayerByteOffset comp layer = LayerByteOffset Int
makeLenses ''LayerByteOffset


-- === Instances === --

instance (Typeable comp, Typeable layer)
      => Show (LayerByteOffset comp layer) where
    showsPrec d (unwrap -> a) = showParen' d $ showString name . showsPrec' a
        where name = (<> " ") $ unwords
                   [ "LayerByteOffset"
                   , '@' : show (typeRep @comp)
                   , '@' : show (typeRep @layer)
                   ]



------------------------
-- === Pass State === --
------------------------

-- === Definition === --

newtype     State       pass = State (StateData pass)
type        StateData   pass = TypeMap (StateLayout pass)
type family StateLayout pass :: [Type] -- CACHED WITH OCI.Pass.Cache.define
type ComputeStateLayout pass
    = MapLayerByteOffset pass             (Vars pass Elems)
   <> List.Map Attr                       (Vars pass Attrs)
   <> MapComponentMemPool                 (Vars pass Elems)
   <> MapComponentByteSize                (Vars pass Elems)
   <> List.Map Component.DynamicTraversal (Vars pass Elems)
   <> List.Map Layer.DynamicManager       (Vars pass Elems)
   <> '[CompiledIRInfo]

type MapLayerByteOffset p c = MapOverCompsAndVars LayerByteOffset p c

type family MapComponentMemPool ls where
    MapComponentMemPool '[]       = '[]
    MapComponentMemPool (l ': ls) = MemPool (Component l ())
                                 ': MapComponentMemPool ls

type family MapComponentByteSize ls where
    MapComponentByteSize '[]       = '[]
    MapComponentByteSize (l ': ls) = ByteSize (Component l)
                                  ': MapComponentByteSize ls

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



------------------------
-- === MonadState === --
------------------------

-- | Local State Monad. It is just a nice interface for accessing elements
--   handling all needed special cases.

-- === Definition === --

class Monad m => Getter pass a m where get :: m a
class Monad m => Setter pass a m where put :: a -> m ()

instance {-# OVERLAPPABLE #-}
    ( Monad m
    , TypeMap.ElemGetter a (StateLayout pass)
    , State.Getter (State pass) m)
    => Getter pass a m where
    get = TypeMap.getElem @a . unwrap <$> State.get @(State pass) ; {-# INLINE get #-}

instance {-# OVERLAPPABLE #-}
    ( Monad m
    , TypeMap.ElemSetter a (StateLayout pass)
    , State.Monad (State pass) m)
    => Setter pass a m where
    put t = State.modify_ @(State pass) $ wrapped %~ TypeMap.setElem t ; {-# INLINE put #-}

instance (Getter pass CompiledIRInfo m, Monad m)
      => Getter pass Component.DynamicTraversalMap m where
    get = wrap . fmap (view IRInfo.layersComponents)
               . view IRInfo.compiledComponents
      <$> get @pass @CompiledIRInfo
    {-# INLINE get #-}
