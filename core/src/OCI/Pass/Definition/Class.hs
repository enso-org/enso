{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition.Class where

import Prologue hiding (FromList)

import qualified Control.Monad.State.Layered as State
import qualified Data.Graph.Data             as Component
import qualified Data.Graph.Data.Graph.Class as Graph
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.TypeMap.MultiState     as MultiState
import qualified Data.TypeMap.Strict         as TypeMap
import qualified Type.Data.List              as List

import Control.Monad.State.Layered     (StateT)
import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Graph.Class     (Graph)
import Data.TypeMap.MultiState         (MultiStateT)
import Data.TypeMap.Strict             (TypeMap)
import Foreign.Info.ByteSize           (ByteSize)
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import OCI.Pass.Definition.Declaration (Attrs, Vars)
import OCI.Pass.State.Attr             (Attr)
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



------------------
-- === Pass === --
------------------

-- === Definition === --

type    Env  stage = Graph stage
newtype Pass stage pass a = Pass (MultiStateT (StateLayout pass) (Env stage) a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow)
makeLenses ''Pass

class Definition stage pass where
    definition :: Pass stage pass ()


-- === API === --

exec :: ∀ stage pass a. Pass stage pass a -> State pass -> Env stage (State pass)
exec !pass !state = wrap <$> MultiState.execT (coerce pass) (unwrap state)
{-# INLINE exec #-}

eval :: ∀ stage pass a. Pass stage pass a -> State pass -> Env stage ()
eval !pass !state = void $ exec pass state
{-# INLINE eval #-}


-- === State === --

instance State.Getter a (MultiStateT (StateLayout pass) (Env stage))
      => State.Getter a (Pass stage pass) where
     get = wrap $ State.get @a ; {-# INLINE get #-}

instance State.Setter a (MultiStateT (StateLayout pass) (Env stage))
      => State.Setter a (Pass stage pass) where
     put = wrap . State.put @a ; {-# INLINE put #-}



-- === Instances === --

type instance Graph.Discover (Pass stage pass) = stage

instance Layer.KnownLayer t layer (Env stage)
      => Layer.KnownLayer t layer (Pass stage pass) where
    layerByteOffset = Layer.layerByteOffset @t @layer @(Env stage)
    {-# INLINE layerByteOffset #-}



-----------------
-- === Rep === --
-----------------

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep

rep :: ∀ (pass :: Type). Typeable pass => Rep
rep = wrap $ someTypeRep @pass ; {-# INLINE rep #-}

repOf :: ∀ stage pass a. Typeable pass => Pass stage pass a -> Rep
repOf _ = rep @pass ; {-# INLINE repOf #-}

