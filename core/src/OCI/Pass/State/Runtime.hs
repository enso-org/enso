{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.State.Runtime where

import Prologue

import qualified Control.Monad.State.Layered       as State
import qualified Data.Graph.Component.Edge.Class   as Edge
import qualified Data.Graph.Data                   as Component
import qualified Data.Graph.Data.Component.Dynamic as Component
import qualified Data.Graph.Data.Layer.Class       as Layer
import qualified Data.TypeMap.Strict               as TypeMap
import qualified OCI.Pass.State.IRInfo             as IRInfo
import qualified Type.Data.List                    as List

import Control.Monad.State.Layered     (StateT)
import Data.Graph.Class                (LayerByteOffset)
import Data.Graph.Data.Component.Class (Component)
import Data.TypeMap.Strict             (TypeMap)
import Foreign.Info.ByteSize           (ByteSize)
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import OCI.IR.Term                     (Terms)
import OCI.Pass.Definition.Declaration (Attrs, Elems, Vars)
import OCI.Pass.State.Attr             (Attr)
import OCI.Pass.State.IRInfo           (CompiledIRInfo)
import Type.Data.List                  (type (<>))



------------------------
-- === Pass State === --
------------------------

-- === Definition === --

newtype State       pass = State (StateData pass)
type    StateData   pass = TypeMap (StateLayout pass)
type    StateLayout pass = List.Map Attr (Vars pass Attrs)
makeLenses ''State


-- === Instances === --

deriving instance Show    (StateData pass) => Show    (State pass)
deriving instance Default (StateData pass) => Default (State pass)

