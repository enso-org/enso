{-# LANGUAGE UndecidableInstances #-}

module Luna.Compilation.Env.Class where

import Prelude.Luna

import qualified Control.Monad.State            as State
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import qualified Luna.Diagnostic.Info           as Info
import           Data.Build
import           Data.Version.Semantic



-- === Definitions === --

data Cfg = Cfg { __version :: Version
               , __build   :: Build
               } deriving (Show)
makeLenses ''Cfg

instance Default Cfg where def = Cfg Info.version Info.build

instance HasVersion Cfg where version = _version


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Env      = EnvT Identity
newtype EnvT m a = EnvT (State.StateT Cfg m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''EnvT


-- === Utils === --

runT  ::            EnvT m a -> Cfg -> m (a, Cfg)
evalT :: Monad m => EnvT m a -> Cfg -> m a
execT :: Monad m => EnvT m a -> Cfg -> m Cfg

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Env a -> Cfg -> (a, Cfg)
eval :: Env a -> Cfg -> a
exec :: Env a -> Cfg -> Cfg

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadEnv m => (Cfg -> Cfg) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadEnv m => (Cfg -> (a, Cfg)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadEnv m => (Cfg -> m (a, Cfg)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadEnv m => (Cfg -> Cfg) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadEnv m where
    get :: m Cfg
    put :: Cfg -> m ()

instance Monad m => MonadEnv (EnvT m) where
    get = EnvT   State.get ; {-# INLINE get #-}
    put = EnvT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (EnvT m) where
    get = EnvT $ lift   State.get ; {-# INLINE get #-}
    put = EnvT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadEnv m, MonadTrans t, Monad (t m)) => MonadEnv (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<
