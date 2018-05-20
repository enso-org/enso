{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition.Class where

import Prologue hiding (FromList)

import qualified Control.Monad.State.Layered       as State
import qualified Data.Graph.Class                  as Graph
import qualified Data.Graph.Data                   as Component
import qualified Data.Graph.Data.Component.Dynamic as Component
import qualified Data.Graph.Data.Layer.Class       as Layer
import qualified Data.TypeMap.MultiState           as MultiState
import qualified Data.TypeMap.Strict               as TypeMap
import qualified Type.Data.List                    as List

import Control.Monad.State.Layered     (StateT)
import Data.Graph.Class                (Graph)
import Data.Graph.Data.Component.Class (Component)
import Data.TypeMap.MultiState         (MultiStateT)
import Data.TypeMap.Strict             (TypeMap)
import Foreign.Info.ByteSize           (ByteSize)
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import OCI.Pass.Definition.Declaration (Attrs, FromList, StageOf, Vars)
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

type    Env  pass = Graph (StageOf pass)
newtype Pass pass a = Pass (MultiStateT (StateLayout pass) (Env pass) a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow)
makeLenses ''Pass

class Definition pass where
    definition :: Pass pass ()


-- === API === --

exec :: ∀ pass a. Pass pass a -> State pass -> Env pass (State pass)
exec !pass !state = wrap <$> MultiState.execT (coerce pass) (unwrap state)
{-# INLINE exec #-}

eval :: ∀ pass a. Pass pass a -> State pass -> Env pass ()
eval !pass !state = void $ exec pass state
{-# INLINE eval #-}


-- === State === --

instance State.Getter a (MultiStateT (StateLayout pass) (Env pass))
      => State.Getter a (Pass pass) where
     get = wrap $ State.get @a ; {-# INLINE get #-}

instance State.Setter a (MultiStateT (StateLayout pass) (Env pass))
      => State.Setter a (Pass pass) where
     put = wrap . State.put @a ; {-# INLINE put #-}


-- === Layer Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData layer
    , State.Getter (Graph.LayerByteOffset comp layer) (Pass pass)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Reader (Component comp) layer (Pass pass) where
    read__ !comp = do
        !off <- unwrap <$> State.get @(Graph.LayerByteOffset comp layer)
        Layer.unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData layer
    , State.Getter (Graph.LayerByteOffset comp layer) (Pass pass)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Writer (Component comp) layer (Pass pass) where
    write__ !comp !d = do
        !off <- unwrap <$> State.get @(Graph.LayerByteOffset comp layer)
        Layer.unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}


-- === Layer.View Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableView layer layout
    , State.Getter (Graph.LayerByteOffset comp layer) (Pass pass)
    ) => Layer.ViewReader (Component comp) layer layout (Pass pass) where
    readView__ !comp = do
        !off <- unwrap <$> State.get @(Graph.LayerByteOffset comp layer)
        Layer.unsafeReadViewByteOff @layer off comp
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableView layer layout
    , State.Getter (Graph.LayerByteOffset comp layer) (Pass pass)
    ) => Layer.ViewWriter (Component comp) layer layout (Pass pass) where
    writeView__ !comp !d = do
        !off <- unwrap <$> State.get @(Graph.LayerByteOffset comp layer)
        Layer.unsafeWriteViewByteOff @layer off comp d
    {-# INLINE writeView__ #-}


-- === Instances === --

type instance Graph.Discover (Pass pass) = StageOf pass



-----------------
-- === Rep === --
-----------------

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep

rep :: ∀ (pass :: Type). Typeable pass => Rep
rep = wrap $ someTypeRep @pass ; {-# INLINE rep #-}

repOf :: ∀ stage pass a. Typeable pass => Pass pass a -> Rep
repOf _ = rep @pass ; {-# INLINE repOf #-}

