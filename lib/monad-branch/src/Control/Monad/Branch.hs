module Control.Monad.Branch where

import Prelude

import Control.Monad.Identity
import qualified Control.Monad.Trans.State        as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict


-------------------------
-- === MonadBranch === --
-------------------------

-- === Definition === --

class Monad m => MonadBranch m where
    branched :: m a -> m a


-- === Standard instances === --

instance MonadBranch Identity where
    branched = id

instance MonadBranch m => MonadBranch (IdentityT m) where
    branched = mapIdentityT branched

instance MonadBranch m => MonadBranch (Lazy.StateT s m) where
    branched (Lazy.StateT m) = Lazy.StateT $ \s -> (,s) . fst <$> branched (m s)

instance MonadBranch m => MonadBranch (Strict.StateT s m) where
    branched (Strict.StateT m) = Strict.StateT $ \s -> (,s) . fst <$> branched (m s)

