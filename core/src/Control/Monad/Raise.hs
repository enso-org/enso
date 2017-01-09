{-# LANGUAGE TypeInType #-}

module Control.Monad.Raise where

import Prelude
import Data.Kind

import Control.Lens.Utils
import Control.Exception   (Exception, SomeException, toException)
import Control.Monad.Catch (MonadThrow, throwM)

import Control.Monad              (join)
import Data.Constraint            (Constraint)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans        (MonadTrans, lift)


-------------------------------
-- === Exception raising === --
-------------------------------

-- === MonadException === --

type ExceptT' = ExceptT SomeException

class (Monad m, Exception e) => MonadException e m where
    raise :: forall a. e -> m a

type family MonadExceptions es m :: Constraint where
    MonadExceptions '[]       m = ()
    MonadExceptions (e ': es) m = (MonadException e m, MonadExceptions es m)


-- === Utils === --

handle :: Monad m => (e -> m a) -> ExceptT e m a -> m a
handle f = join . fmap (either f return) . runExceptT ; {-# INLINE handle #-}

handleAll :: Monad m => (SomeException -> m a) -> ExceptT' m a -> m a
handleAll = handle ; {-# INLINE handleAll #-}

rethrow :: (MonadThrow m, Exception e) => ExceptT e m a -> m a
rethrow = handle throwM ; {-# INLINE rethrow #-}

rethrowAll :: MonadThrow m => ExceptT' m a -> m a
rethrowAll = rethrow ; {-# INLINE rethrowAll #-}


-- === Throws === --

type family   Throws (e :: k) (m :: * -> *) :: Constraint
type instance Throws e m = MonadExceptions e m
type instance Throws e m = MonadException  e m


-- === Intsances === --

-- Default MonadException instances
instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadException e m)
                                                     => MonadException e (t                     m) where raise = lift . raise         ; {-# INLINE raise #-}
instance {-# OVERLAPPABLE #-} (Monad m, Exception e) => MonadException e (ExceptT e             m) where raise = throwE               ; {-# INLINE raise #-}
instance                      (Monad m, Exception e) => MonadException e (ExceptT SomeException m) where raise = throwE . toException ; {-# INLINE raise #-}



-- === Utils === --

tryJust :: MonadException e m => e -> Maybe a -> m a
tryJust e = maybe (raise e) return ; {-# INLINE tryJust #-}
