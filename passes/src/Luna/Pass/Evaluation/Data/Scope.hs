{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Luna.Pass.Evaluation.Data.Scope where

import Prologue

import qualified Control.Monad.State.Layered           as State
import qualified Data.Map                              as Map
import qualified Luna.IR                               as IR
import qualified Luna.Runtime                          as Runtime

import Data.IORef          (IORef, readIORef, writeIORef)
import Data.Map            (Map)



-------------------------
-- === Local Scope === --
-------------------------

-- === Definition === --

newtype LocalScope = LocalScope
    { _localVars :: Map IR.SomeTerm Runtime.Data }
    deriving Default
makeLenses ''LocalScope

-- === API === --

localLookup :: IR.SomeTerm -> LocalScope -> Maybe Runtime.Data
localLookup e = Map.lookup e . view localVars

localInsert :: IR.SomeTerm -> Runtime.Data -> LocalScope -> LocalScope
localInsert e d = localVars %~ Map.insert e d

merge :: Map IR.SomeTerm Runtime.Data -> LocalScope -> LocalScope
merge m = localVars %~ Map.union m



-------------------------
-- === Scope Monad === --
-------------------------

type MonadScope m = State.Monad LocalScope m



----------------------------
-- === Mutable ScopeT === --
----------------------------

newtype MutableScopeT m a = MutableScopeT
    { unMutableScopeT :: State.StateT (IORef LocalScope) m a }
    deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- === API === --

evalWithRef :: Monad m => MutableScopeT m a -> IORef LocalScope -> m a
evalWithRef = State.evalT . unMutableScopeT
{-# INLINE evalWithRef #-}

-- === Instances === --

instance MonadIO m => State.Getter LocalScope (MutableScopeT m) where
    get = MutableScopeT $ State.get @(IORef LocalScope) >>= liftIO . readIORef
    {-# INLINE get #-}

instance MonadIO m => State.Setter LocalScope (MutableScopeT m) where
    put = \s -> MutableScopeT $ State.get @(IORef LocalScope)
                >>= liftIO . flip writeIORef s
    {-# INLINE put #-}



--------------------
-- === ScopeT === --
--------------------

type ScopeT m a = State.StateT LocalScope m a

