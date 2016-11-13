{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delayed where

import Prologue
import Control.Monad.State (StateT, withState, runStateT)

import qualified Control.Monad.State as State



---------------------
-- === Delayed === --
---------------------

class                         Monad m          => MonadDelayed m          where delayed :: Delayed m () -> m ()
instance {-# OVERLAPPABLE #-} (Monad m, n ~ m) => MonadDelayed (Runner m) where delayed = Runner . State.modify . (<*) ; {-# INLINE delayed #-}
instance {-# OVERLAPPABLE #-} Redelayed t m    => MonadDelayed (t m)      where delayed = redelayed                    ; {-# INLINE delayed #-}

type family Delayed m where
    Delayed (Runner m) = m
    Delayed (t m)      = Delayed m

type Redelayed t m = ( MonadDelayed m
                     , MonadTrans t
                     , Monad (t m)
                     , Delayed (t m) ~ Delayed m
                     )
redelayed :: Redelayed t m => Delayed m () -> t m ()
redelayed = lift ∘ delayed ; {-# INLINE redelayed #-}



--------------------
-- === Runner === --
--------------------

newtype Runner m a = Runner (StateT (m ()) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''Runner


-- === Running === --

run :: Monad m => Runner m a -> m a
run (unwrap' -> s) = do
    (a, f) <- runStateT s (return ())
    a <$ f

eval' :: Monad m => Runner m a -> m a
eval' = run


-- === Instances === --

-- MonadTrans
instance MonadTrans Runner where
    lift = Runner ∘ lift ; {-# INLINE lift #-}

-- PrimMonad
instance PrimMonad m => PrimMonad (Runner m) where
    type PrimState (Runner m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}
