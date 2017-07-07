module Control.Monad.Branch where

import Prelude
import Control.Monad.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.State


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

instance MonadBranch m => MonadBranch (StateT s m) where
    branched (StateT m) = StateT $ \s -> (,s) . fst <$> branched (m s)
