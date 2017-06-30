{-# LANGUAGE UndecidableInstances #-}
{-# EXT      InlineAll            #-}

module Control.Monad.Runner where

import Prelude

import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Control.Monad.State
import Data.Default


-----------------
-- MonadRunner --
-----------------

-- Definition --

class Monad m => MonadRunner m where
    run :: m a -> a

class MonadTrans t => MonadTransRunner t where
    runT :: forall m a. Monad m => t m a -> m a


-- Instances --

instance {-# OVERLAPPABLE #-} (MonadTransRunner t, MonadRunner m, Monad (t m))
      => MonadRunner (t m) where
    run = run . runT


-- Std Instances --

instance MonadRunner      Identity  where run  = runIdentity
instance MonadTransRunner IdentityT where runT = runIdentityT

instance {-# OVERLAPPABLE #-} Default s => MonadRunner      (State s)  where run  = flip evalState  def
instance {-# OVERLAPPABLE #-} Default s => MonadTransRunner (StateT s) where runT = flip evalStateT def
