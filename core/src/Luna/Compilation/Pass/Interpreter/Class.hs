{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Compilation.Pass.Interpreter.Class where

import           Prologue

import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import qualified Control.Monad.State as State


-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

type    Interpreter  env     = InterpreterT env Identity
newtype InterpreterT env m a = InterpreterT { fromInterpreterT :: State.StateT env m a }
                                            deriving ( Functor
                                                     , Monad
                                                     , Applicative
                                                     , MonadIO
                                                     , MonadPlus
                                                     , MonadTrans
                                                     , Alternative
                                                     , MonadFix
                                                     , MonadMask
                                                     , MonadCatch
                                                     , MonadThrow
                                                     )

makeWrapped ''InterpreterT


-- === Instances === --

class Monad m => InterpreterMonad env m | m -> env where
    get :: m env
    put :: env -> m ()

instance Monad m => InterpreterMonad env (InterpreterT env m) where
    get = InterpreterT State.get
    put = InterpreterT . State.put

instance State.MonadState s m => State.MonadState s (InterpreterT env m) where
    get = InterpreterT (lift State.get)
    put = InterpreterT . lift . State.put

instance {-# OVERLAPPABLE #-} (InterpreterMonad env m, MonadTrans t, Monad (t m)) => InterpreterMonad env (t m) where
    get = lift get
    put = lift . put


-- === Utils === --

runT  ::            InterpreterT env m a -> env -> m (a, env)
evalT :: Monad m => InterpreterT env m a -> env -> m a
execT :: Monad m => InterpreterT env m a -> env -> m env

runT  = State.runStateT  . fromInterpreterT ; {-# INLINE runT  #-}
evalT = State.evalStateT . fromInterpreterT ; {-# INLINE evalT #-}
execT = State.execStateT . fromInterpreterT ; {-# INLINE execT #-}


run  :: Interpreter env a -> env -> (a, env)
eval :: Interpreter env a -> env -> a
exec :: Interpreter env a -> env -> env

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: InterpreterMonad env m => (env -> env) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: InterpreterMonad env m => (env -> (env, a)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modify2 :: InterpreterMonad env m => (env -> (a, env)) -> m a
modify2 = modifyM2 . fmap return
{-# INLINE modify2 #-}

modifyM :: InterpreterMonad env m => (env -> m (env, a)) -> m a
modifyM f = do
    s <- get
    (s', a) <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modifyM2 :: InterpreterMonad env m => (env -> m (a, env)) -> m a
modifyM2 f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM2 #-}

modify_ :: InterpreterMonad env m => (env -> env) -> m ()
modify_ = modify . fmap (,())
{-# INLINE modify_ #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<


withEnv :: InterpreterMonad env m => (env -> (env, a)) -> m a
withEnv = withEnvM . fmap return

withEnv' :: InterpreterMonad env m => (env -> (a, env)) -> m a
withEnv' = withEnvM . fmap (return . switch')

withEnv_ :: InterpreterMonad env m => (env -> env) -> m ()
withEnv_ = withEnv . fmap (,())

withEnvM :: InterpreterMonad env m => (env -> m (env, a)) -> m a
withEnvM = modifyM

withEnvM_ :: InterpreterMonad env m => (env -> m env) -> m ()
withEnvM_ = withEnvM . (fmap . fmap) (,())

runInterpreterT  :: Functor m => InterpreterT env m a -> env -> m (a, env)
execInterpreterT :: Monad   m => InterpreterT env m a -> env -> m env
evalInterpreterT :: Monad   m => InterpreterT env m a -> env -> m a

runInterpreterT  = runT
execInterpreterT = execT
evalInterpreterT = evalT


switch' (a,b) = (b,a)
