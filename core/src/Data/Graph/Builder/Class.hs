{-# LANGUAGE UndecidableInstances #-}


module Data.Graph.Builder.Class where

import Prologue hiding (Getter, Setter, read, (#))

import           Data.Prop
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import           Data.Construction
import           Data.Container
import           Data.Index
import qualified Control.Monad.State            as State
import           Data.Layer.Cover


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Builder  g     = BuilderT g Identity
newtype BuilderT g m a = BuilderT (State.StateT g m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''BuilderT


-- === Utils === --

runT  ::            BuilderT g m a -> g -> m (a, g)
evalT :: Monad m => BuilderT g m a -> g -> m a
execT :: Monad m => BuilderT g m a -> g -> m g

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Builder g a -> g -> (a, g)
eval :: Builder g a -> g -> a
exec :: Builder g a -> g -> g

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadBuilder g m => (g -> g) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadBuilder g m => (g -> (a, g)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadBuilder g m => (g -> m (a, g)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadBuilder g m => (g -> g) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadBuilder g m | m -> g where
    get :: m g
    put :: g -> m ()

instance Monad m => MonadBuilder g (BuilderT g m) where
    get = BuilderT   State.get ; {-# INLINE get #-}
    put = BuilderT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (BuilderT g m) where
    get = BuilderT $ lift   State.get ; {-# INLINE get #-}
    put = BuilderT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadBuilder g m, MonadTrans t, Monad (t m)) => MonadBuilder g (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<
