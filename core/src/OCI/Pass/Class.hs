{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Class where

import Prologue

import qualified Control.Monad.State.Layered  as State
import qualified Data.Graph.Component         as Component
import qualified Data.Graph.Component.Dynamic as Component
import qualified Data.Graph.Component.Layer   as Layer
import qualified Data.TypeMap.Strict          as TypeMap
import qualified Type.Data.List               as List

import Control.Monad.State.Layered (StateT)
import Data.Graph.Component.Class  (Component)
import Data.TypeMap.Strict         (TypeMap)
import Foreign.Info.ByteSize       (ByteSize)
import Foreign.Memory.Pool         (MemPool)
import Foreign.Ptr.Utils           (SomePtr)
import OCI.Pass.Attr               (Attr)
import OCI.Pass.Definition
import OCI.Pass.State.Runtime
import Type.Data.List              (type (<>))



------------------
-- === Pass === --
------------------

-- === Definition === --

newtype Pass (pass :: Type) a = Pass (StateT (State pass) IO a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow)
makeLenses ''Pass

class Definition pass where
    definition :: Pass pass ()


-- === Discovery === --

type family DiscoverPass m where
    DiscoverPass (Pass pass) = pass
    DiscoverPass (t m)       = DiscoverPass m

type DiscoverState       m = State       (DiscoverPass m)
type DiscoverStateData   m = StateData   (DiscoverPass m)
type DiscoverStateLayout m = StateLayout (DiscoverPass m)


-- === API === --

exec :: ∀ pass a. Pass pass a -> State pass -> IO (State pass)
exec !pass !state = State.execT (coerce pass) state ; {-# INLINE exec #-}

eval :: ∀ pass a. Pass pass a -> State pass -> IO ()
eval !pass !state = void $ exec pass state ; {-# INLINE eval #-}


-- === State === --

instance State.Getter (State pass) (Pass pass) where
    get = wrap State.get' ; {-# INLINE get #-}

instance State.Setter (State pass) (Pass pass) where
    put = wrap . State.put' ; {-# INLINE put #-}


-- === Layer Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData layer
    , State.Getter (LayerByteOffset comp layer) (Pass pass)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Reader (Component comp) layer (Pass pass) where
    read__ !comp = do
        !off <- unwrap <$> State.get @(LayerByteOffset comp layer)
        Layer.unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableData layer
    , State.Getter (LayerByteOffset comp layer) (Pass pass)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Writer (Component comp) layer (Pass pass) where
    write__ !comp !d = do
        !off <- unwrap <$> State.get @(LayerByteOffset comp layer)
        Layer.unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}


-- === Layer.View Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableView layer layout
    , State.Getter (LayerByteOffset comp layer) (Pass pass)
    ) => Layer.ViewReader (Component comp) layer layout (Pass pass) where
    readView__ !comp = do
        !off <- unwrap <$> State.get @(LayerByteOffset comp layer)
        Layer.unsafeReadViewByteOff @layer off comp
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( Layer.StorableView layer layout
    , State.Getter (LayerByteOffset comp layer) (Pass pass)
    ) => Layer.ViewWriter (Component comp) layer layout (Pass pass) where
    writeView__ !comp !d = do
        !off <- unwrap <$> State.get @(LayerByteOffset comp layer)
        Layer.unsafeWriteViewByteOff @layer off comp d
    {-# INLINE writeView__ #-}



-----------------
-- === Rep === --
-----------------

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep

rep :: ∀ (pass :: Type). Typeable pass => Rep
rep = wrap $ someTypeRep @pass ; {-# INLINE rep #-}

repOf :: ∀ pass a. Typeable pass => Pass pass a -> Rep
repOf _ = rep @pass ; {-# INLINE repOf #-}



-------------------------
--- === MonadState === --
-------------------------

-- === Definition === --

type MonadState m = State.Monad (DiscoverState m) m


-- === API === --

class Monad m => DataGetter a m where getData :: m a
class Monad m => DataSetter a m where putData :: a -> m ()

instance Monad m => DataGetter Imp m     where getData = impossible
instance Monad m => DataSetter Imp m     where putData = impossible
instance            DataGetter a   ImpM1 where getData = impossible
instance            DataSetter a   ImpM1 where putData = impossible

instance (Monad m, MonadState m, TypeMap.ElemGetter a (DiscoverStateLayout m))
    => DataGetter a m where
    getData = TypeMap.getElem @a . unwrap <$> State.get @(DiscoverState m) ; {-# INLINE getData #-}

instance (Monad m, MonadState m, TypeMap.ElemSetter a (DiscoverStateLayout m))
    => DataSetter a m where
    putData a = State.modify_ @(DiscoverState m) $ wrapped %~ TypeMap.setElem a ; {-# INLINE putData #-}


-- === Accessing pass data === --

instance {-# OVERLAPPABLE #-} DataGetter t (Pass pass)
    => State.Getter t (Pass pass) where
    get = getData @t ; {-# INLINE get #-}

instance {-# OVERLAPPABLE #-} DataSetter t (Pass pass)
    => State.Setter t (Pass pass) where
    put = putData @t ; {-# INLINE put #-}
