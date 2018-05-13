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
-- === MultiState === --
------------------------

-- === Definition === --

newtype MultiStateT (s :: [Type]) m a = MultiStateT (StateT (TypeMap s) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow, MonadTrans)
makeLenses ''MultiStateT


-- === API === --

runT  ::              MultiStateT s m a -> TypeMap s -> m (a, TypeMap s)
execT :: Functor m => MultiStateT s m a -> TypeMap s -> m (TypeMap s)
evalT :: Functor m => MultiStateT s m a -> TypeMap s -> m a
runT  = State.runT  . unwrap ; {-# INLINE runT  #-}
execT = State.execT . unwrap ; {-# INLINE execT #-}
evalT = State.evalT . unwrap ; {-# INLINE evalT #-}


-- === State instances === --

instance (elem ~ List.In a s, Getter__ elem s m a)
      => State.Getter a (MultiStateT s m) where
    get = get__ @elem ; {-# INLINE get #-}

instance (elem ~ List.In a s, Setter__ elem s m a)
      => State.Setter a (MultiStateT s m) where
    put = put__ @elem ; {-# INLINE put #-}

class Monad m => Getter__ (elem :: Bool) s m a where
    get__ :: MultiStateT s m a

class Monad m => Setter__ (elem :: Bool) s m a where
    put__ :: a -> MultiStateT s m ()

instance (Monad m, State.Getter a m) => Getter__ 'False s m a where
    get__ = lift $ State.get @a ; {-# INLINE get__ #-}

instance (Monad m, State.Setter a m) => Setter__ 'False s m a where
    put__ = lift . State.put @a ; {-# INLINE put__ #-}

instance (Monad m, TypeMap.ElemGetter a s)
      => Getter__ 'True s m a where
    get__ = wrap $ TypeMap.getElem @a <$> State.get @(TypeMap s)
    {-# INLINE get__ #-}

instance (Monad m, TypeMap.ElemSetter a s)
      => Setter__ 'True s m a where
    put__ a = wrap $ State.modify_ @(TypeMap s) $ TypeMap.setElem @a a
    {-# INLINE put__ #-}






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
   <> '[CompiledIRInfo, Edge.ComponentProvider Terms]

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



-- ------------------------
-- -- === MonadState === --
-- ------------------------

-- -- | Local State Monad. It is just a nice interface for accessing elements
-- --   handling all needed special cases.

-- -- === Definition === --

-- class Monad m => Getter pass a m where get :: m a
-- class Monad m => Setter pass a m where put :: a -> m ()

-- instance {-# OVERLAPPABLE #-}
--     ( Monad m
--     , TypeMap.ElemGetter a (StateLayout pass)
--     , State.Getter (State pass) m)
--     => Getter pass a m where
--     get = TypeMap.getElem @a . unwrap <$> State.get @(State pass) ; {-# INLINE get #-}

-- instance {-# OVERLAPPABLE #-}
--     ( Monad m
--     , TypeMap.ElemSetter a (StateLayout pass)
--     , State.Monad (State pass) m)
--     => Setter pass a m where
--     put t = State.modify_ @(State pass) $ wrapped %~ TypeMap.setElem t ; {-# INLINE put #-}

-- instance (Getter pass CompiledIRInfo m, Monad m)
--       => Getter pass Component.DynamicTraversalMap m where
--     get = wrap . fmap (view IRInfo.layersComponents)
--                . view IRInfo.compiledComponents
--       <$> get @pass @CompiledIRInfo
--     {-# INLINE get #-}
