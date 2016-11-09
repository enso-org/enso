{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delayed2 where

import Prologue
import Control.Monad.State (StateT, withState, runStateT)

import qualified Control.Monad.State as State


-- === Definitions === --

newtype Delayed m a = Delayed (StateT (m ()) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''Delayed

class MonadDelayed n m | m -> n where
    delay :: n () -> m ()


-- === Utils === --

run :: Monad m => Delayed m a -> m a
run (unwrap' -> s) = do
    (a, f) <- runStateT s (return ())
    a <$ f

eval' :: Monad m => Delayed m a -> m a
eval' = run


-- === Instances === --

instance {-# OVERLAPPABLE #-} (Monad m, n ~ m) => MonadDelayed n (Delayed m) where
    delay act = Delayed $ State.modify (<* act) ; {-# INLINE delay #-}

instance {-# OVERLAPPABLE #-} (MonadDelayed n m, MonadTrans t, Monad m) => MonadDelayed n (t m) where
    delay = lift ∘ delay ; {-# INLINE delay #-}

instance MonadTrans (Delayed) where
    lift = Delayed ∘ lift ; {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (Delayed m) where
    type PrimState (Delayed m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}
