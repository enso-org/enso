{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Class where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Data.TypeMap.Strict         as TypeMap
import qualified OCI.Pass.State.Runtime      as MultiState

import OCI.Pass.State.Runtime (MultiStateT)


type family Components      tag      :: [Type]
type family ComponentLayers tag comp :: [Type]

type StateData tag = '[]
type State tag = TypeMap.TypeMap (StateData tag)






type family DiscoverGraphTag (m :: Type -> Type) :: Type -- where
    -- DiscoverGraphTag (Graph tag m) = tag
    -- DiscoverGraphTag (t m)         = DiscoverGraphTag m

type DiscoverComponents      m = Components (DiscoverGraphTag m)
type DiscoverComponentLayers m comp = ComponentLayers (DiscoverGraphTag m) comp



-------------------
-- === Graph === --
-------------------

-- === Definition === --

type    Graph  tag     = GraphT tag IO
newtype GraphT tag m a = GraphT (MultiStateT (StateData tag) m a)
    deriving ( Applicative, Alternative, Functor, Monad, MonadFail, MonadFix
             , MonadIO, MonadPlus, MonadThrow, MonadTrans)
makeLenses ''GraphT


-- === API === --

run :: ∀ t m a. Functor m => GraphT t m a -> m a
run g = MultiState.evalT (unwrap g) TypeMap.empty ; {-# INLINE run #-}

-- run  :: ∀ tag m a. Monad m => GraphT tag m a -> State tag -> m (a, State tag)
-- exec :: ∀ tag m a. Monad m => GraphT tag m a -> State tag -> m (State tag)
-- eval :: ∀ tag m a. Monad m => GraphT tag m a -> State tag -> m a
-- run  = MultiState.runT  . unwrap ; {-# INLINE run  #-}
-- exec = MultiState.execT . unwrap ; {-# INLINE exec #-}
-- eval = MultiState.evalT . unwrap ; {-# INLINE eval #-}


-- === State === --

instance (Monad m, State.Getter a (MultiStateT (StateData tag) m))
      => State.Getter a (GraphT tag m) where
     get = wrap $ State.get @a ; {-# INLINE get #-}

instance (Monad m, State.Setter a (MultiStateT (StateData tag) m))
      => State.Setter a (GraphT tag m) where
     put = wrap . State.put @a ; {-# INLINE put #-}

