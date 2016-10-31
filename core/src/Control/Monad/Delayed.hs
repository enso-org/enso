{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delayed where

import Prologue hiding (s)
import Control.Monad.State (StateT, runStateT)

import qualified Control.Monad.State as State


-- === Definitions === --

type    Delayed' = Delayed ()
newtype Delayed t m a = Delayed (StateT [m t] m a) deriving (Functor, Applicative, Monad)
makeWrapped ''Delayed

class MonadDelayed n t m | m -> n t where
    delayed :: n t -> m ()

-- === Utils === --

run :: Monad m => Delayed t m a -> m (a, [t])
run (unwrap' -> s) = runStateT s mempty >>= mapM (sequence ∘ reverse)

eval :: Monad m => Delayed t m a -> m a
eval = fst <∘> run

exec :: Monad m => Delayed t m a -> m [t]
exec = snd <∘> run

exec_ :: Monad m => Delayed t m a -> m ()
exec_ = void ∘ exec

run' :: Delayed t m a -> m (a, [m t])
run' (unwrap' -> s) = runStateT s mempty

-- === Instances === --

instance Monad m => MonadDelayed m t (Delayed t m) where
    delayed act = Delayed $ State.modify (act :)




instance (MonadDelayed n s m, MonadTrans t, Monad m) => MonadDelayed n s (t m) where
    delayed = lift ∘ delayed


instance MonadTrans (Delayed t) where
    lift = Delayed ∘ lift



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
