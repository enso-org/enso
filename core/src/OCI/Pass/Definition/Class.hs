{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances      #-}

module OCI.Pass.Definition.Class where

import Prologue

import qualified Control.Monad.State.Layered       as State
import qualified Data.Graph.Data                   as Component
import qualified Data.Graph.Data.Component.Dynamic as Component
import qualified Data.Graph.Data.Layer.Class       as Layer
import qualified Data.TypeMap.Strict               as TypeMap
import qualified OCI.Pass.State.Runtime            as Runtime
import qualified Type.Data.List                    as List

import Control.Monad.State.Layered     (StateT)
import Data.Graph.Class                (Graph)
import Data.Graph.Data.Component.Class (Component)
import Data.TypeMap.Strict             (TypeMap)
import Foreign.Info.ByteSize           (ByteSize)
import Foreign.Memory.Pool             (MemPool)
import Foreign.Ptr.Utils               (SomePtr)
import OCI.Pass.State.Attr             (Attr)
import OCI.Pass.State.Runtime          (MultiState)
import Type.Data.List                  (type (<>))






------------------
-- === Pass === --
------------------

-- === Definition === --

newtype Pass pass m a = Pass (StateT (Runtime.State pass) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow)
makeLenses ''Pass

class Definition pass where
    definition :: Pass pass m ()


-- === API === --

exec :: ∀ pass m a. Monad m
     => Pass pass m a -> Runtime.State pass -> m (Runtime.State pass)
exec !pass !state = State.execT (coerce pass) state ; {-# INLINE exec #-}

eval :: ∀ pass m a. Monad m
     => Pass pass m a -> Runtime.State pass -> m ()
eval !pass !state = void $ exec pass state ; {-# INLINE eval #-}


-- === State === --

instance Monad m
      => State.Getter (Runtime.State pass) (Pass pass m) where
    get = wrap State.get' ; {-# INLINE get #-}

instance Monad m
      => State.Setter (Runtime.State pass) (Pass pass m) where
    put = wrap . State.put' ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} Runtime.Getter pass a (Pass pass m)
      => State.Getter a (Pass pass m) where
    get = Runtime.get @pass ; {-# INLINE get #-}

instance {-# OVERLAPPABLE #-} Runtime.Setter pass a (Pass pass m)
      => State.Setter a (Pass pass m) where
    put = Runtime.put @pass ; {-# INLINE put #-}


-- === Layer Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( MonadIO m
    , Layer.StorableData layer
    , State.Getter (Runtime.LayerByteOffset comp layer) (Pass pass m)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Reader (Component comp) layer (Pass pass m) where
    read__ !comp = do
        !off <- unwrap <$> State.get @(Runtime.LayerByteOffset comp layer)
        Layer.unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( MonadIO m
    , Layer.StorableData layer
    , State.Getter (Runtime.LayerByteOffset comp layer) (Pass pass m)
    , Layer.Wrapped (Layer.Cons layer)
    ) => Layer.Writer (Component comp) layer (Pass pass m) where
    write__ !comp !d = do
        !off <- unwrap <$> State.get @(Runtime.LayerByteOffset comp layer)
        Layer.unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}


-- === Layer.View Reader / Writer === --

instance {-# OVERLAPPABLE #-}
    ( MonadIO m
    , Layer.StorableView layer layout
    , State.Getter (Runtime.LayerByteOffset comp layer) (Pass pass m)
    ) => Layer.ViewReader (Component comp) layer layout (Pass pass m) where
    readView__ !comp = do
        !off <- unwrap <$> State.get @(Runtime.LayerByteOffset comp layer)
        Layer.unsafeReadViewByteOff @layer off comp
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( MonadIO m
    , Layer.StorableView layer layout
    , State.Getter (Runtime.LayerByteOffset comp layer) (Pass pass m)
    ) => Layer.ViewWriter (Component comp) layer layout (Pass pass m) where
    writeView__ !comp !d = do
        !off <- unwrap <$> State.get @(Runtime.LayerByteOffset comp layer)
        Layer.unsafeWriteViewByteOff @layer off comp d
    {-# INLINE writeView__ #-}



-----------------
-- === Rep === --
-----------------

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep

rep :: ∀ (pass :: Type). Typeable pass => Rep
rep = wrap $ someTypeRep @pass ; {-# INLINE rep #-}

repOf :: ∀ pass m a. Typeable pass => Pass pass m a -> Rep
repOf _ = rep @pass ; {-# INLINE repOf #-}

