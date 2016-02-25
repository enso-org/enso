{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Compilation.Pass.Dirty.Monad where

import           Prologue

import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.State as State


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

newtype DirtyT env m a = DirtyT { fromDirtyT :: State.StateT env m a }
                               deriving (Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative, MonadFix, Catch.MonadMask, Catch.MonadCatch, Catch.MonadThrow)

type Dirty env = DirtyT env Identity

class Monad m => DirtyMonad env m | m -> env where
    get :: m env
    put :: env -> m ()

instance Monad m => DirtyMonad env (DirtyT env m) where
    get = DirtyT State.get
    put = DirtyT . State.put

instance State.MonadState s m => State.MonadState s (DirtyT env m) where
    get = DirtyT (lift State.get)
    put = DirtyT . lift . State.put

instance {-# OVERLAPPABLE #-} (DirtyMonad env m, MonadTrans t, Monad (t m)) => DirtyMonad env (t m) where
    get = lift get
    put = lift . put

runT  ::            DirtyT env m a -> env -> m (a, env)
evalT :: Monad m => DirtyT env m a -> env -> m a
execT :: Monad m => DirtyT env m a -> env -> m env

runT  = State.runStateT  . fromDirtyT
evalT = State.evalStateT . fromDirtyT
execT = State.execStateT . fromDirtyT


run  :: Dirty env a -> env -> (a, env)
eval :: Dirty env a -> env -> a
exec :: Dirty env a -> env -> env

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: DirtyMonad env m => (env -> env) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: DirtyMonad env m => (env -> (env, a)) -> m a
modify = modifyM . fmap return

modify2 :: DirtyMonad env m => (env -> (a, env)) -> m a
modify2 = modifyM2 . fmap return

modifyM :: DirtyMonad env m => (env -> m (env, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a

modifyM2 :: DirtyMonad env m => (env -> m (a, env)) -> m a
modifyM2 f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a

modify_ :: DirtyMonad env m => (env -> env) -> m ()
modify_ = modify . fmap (,())

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

withEnv :: DirtyMonad env m => (env -> (env, a)) -> m a
withEnv = withEnvM . fmap return

withEnv' :: DirtyMonad env m => (env -> (a, env)) -> m a
withEnv' = withEnvM . fmap (return . switch')

withEnv_ :: DirtyMonad env m => (env -> env) -> m ()
withEnv_ = withEnv . fmap (,())

withEnvM :: DirtyMonad env m => (env -> m (env, a)) -> m a
withEnvM = modifyM

withEnvM_ :: DirtyMonad env m => (env -> m env) -> m ()
withEnvM_ = withEnvM . (fmap . fmap) (,())

runDirtyT  :: Functor m => DirtyT env m a -> env -> m (a, env)
execDirtyT :: Monad   m => DirtyT env m a -> env -> m env
evalDirtyT :: Monad   m => DirtyT env m a -> env -> m a

runDirtyT  = runT
execDirtyT = execT
evalDirtyT = evalT




switch' (a,b) = (b,a)
