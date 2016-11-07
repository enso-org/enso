{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delayed2 where

import Prologue
import Control.Monad.State (StateT, withState, runStateT)

import qualified Control.Monad.State as State


-- === Definitions === --

-- type    Delayed' = Delayed ()
newtype Delayed m a = Delayed (StateT (m ()) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadFix)
makeWrapped ''Delayed

class MonadDelayed n m | m -> n where
    delay :: n () -> m ()
    -- delayed :: [n t]

-- === Utils === --

run :: Monad m => Delayed m a -> m a
run (unwrap' -> s) = do
    (a, f) <- runStateT s (return ())
    f
    return a
    --  >>= mapM (sequence ∘ reverse)
--
-- eval :: Monad m => Delayed t m a -> m a
-- eval = fst <∘> run

eval' :: Monad m => Delayed m a -> m a
eval' = run

-- exec :: Monad m => Delayed t m a -> m [t]
-- exec = snd <∘> run
--
-- exec_ :: Monad m => Delayed t m a -> m ()
-- exec_ = void ∘ exec
--
-- run' :: Delayed t m a -> m (a, [m t])
-- run' (unwrap' -> s) = runStateT s mempty

-- === Instances === --

instance {-# OVERLAPPABLE #-} (Monad m, n ~ m) => MonadDelayed n (Delayed m) where
    delay act = Delayed $ State.modify (<* act)




instance {-# OVERLAPPABLE #-} (MonadDelayed n m, MonadTrans t, Monad m) => MonadDelayed n (t m) where
    delay = lift ∘ delay


instance MonadTrans (Delayed) where
    lift = Delayed ∘ lift



instance PrimMonad m => PrimMonad (Delayed m) where
    type PrimState (Delayed m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}



---- === Definitions === --

--type    Delayed' t m a = Delayed (m t) m a
--newtype Delayed  t m a = Delayed (StateT [t] m a) deriving (Functor, Applicative, Monad)
--makeWrapped ''Delayed

--class MonadDelayed t m | m -> t where
--    delayed :: t -> m ()

---- === Utils === --

--run :: Monad m => (t -> m v) -> Delayed t m a -> m (a, [v])
--run f (unwrap' -> s) = runStateT s mempty >>= mapM (sequence ∘ fmap f ∘ reverse)

--run' :: Monad m => Delayed' v m a -> m (a, [v])
--run' = run id

----eval :: Monad m => Delayed t m a -> m a
----eval = fst <∘> run

----exec :: Monad m => Delayed t m a -> m [t]
----exec = snd <∘> run

----exec_ :: Monad m => Delayed t m a -> m ()
----exec_ = void ∘ exec


---- === Instances === --

--instance Monad m => MonadDelayed t (Delayed t m) where
--    delayed act = Delayed $ State.modify (act :)
