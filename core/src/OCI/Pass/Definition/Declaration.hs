{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition.Declaration where

import Prologue hiding (FromList)

-- import qualified Control.Monad.State.Layered as State
-- import qualified Data.Graph.Data             as Component
-- import qualified Data.Graph.Data.Graph.Class as Graph
-- import qualified Data.Graph.Data.Layer.Class as Layer
-- import qualified Data.TypeMap.Strict         as TypeMap
import qualified Type.Data.List              as List

-- import Control.Monad.State.Layered     (StateT)
-- import Data.Graph.Data.Component.Class (Component)
-- import Data.TypeMap.Strict             (TypeMap)
-- import Foreign.Info.ByteSize           (ByteSize)
-- import Foreign.Memory.Pool             (MemPool)
-- import Foreign.Ptr.Utils               (SomePtr)
-- import OCI.Pass.State.Attr             (Attr)
import Type.Data.List                  (type (<>))



------------------------------
-- === Pass Declaration === --
------------------------------

-- | For example:
--
--   data MyPass
--   type instance Stage MyPass  = Stage1
--   type instance Spec MyPass t = Spec_MyPass t
--   type family   Spec_MyPass t where
--       Spec_MyPass (In Elems) = '[Terms, Links]
--       Spec_MyPass (In Terms) = '[Model, Type]
--       Spec_MyPass (In Links) = '[Source, Target]
--       Spec_MyPass (Out a)    = Spec_MyPass (In a)
--       Spec_MyPass t          = '[]

-- === Definition === --

type family Spec (pass :: Type) (prop :: Type) :: [Type]


----------------------
-- === In / Out === --
----------------------

-- === Selectors === --

data In        a
data Out       a
data Preserves a

data Elems
data Attrs


-- === Values === --

-- data List (lst :: [Type])
-- type family FromList list where FromList (List list) = list


-- === Utils === --

type Ins  pass prop = Resolve In  pass prop
type Outs pass prop = Resolve Out pass prop
type Vars pass prop = List.Unique (Ins pass prop <> Outs pass prop)

type Resolve t pass prop = Spec pass (t prop)
