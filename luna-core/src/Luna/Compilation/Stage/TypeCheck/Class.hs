{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Compilation.Stage.TypeCheck.Class where

import Prologue

import qualified Control.Monad.State            as State
import           Control.Monad.Catch            (MonadMask, MonadCatch, MonadThrow)
import           Data.Graph.Model               (Ref, Node)

-- === Definitions === --

data TCState n = TCState { _unresolvedUnis    :: ![Ref Node n]
                         , _unresolvedSubs    :: ![Ref Node n]
                         , _unresolvedSymbols :: ![Ref Node n]
                         , _untypedApps       :: ![Ref Node n]
                         , _untypedAccs       :: ![Ref Node n]
                         , _untypedLits       :: ![Ref Node n]
                         } deriving (Show, Eq)

makeLenses ''TCState

instance Default (TCState n) where
    def = TCState def def def def def def


---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    TypeCheck  n     = TypeCheckT n Identity
newtype TypeCheckT n m a = TypeCheckT (State.StateT (TCState n) m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''TypeCheckT


-- === Utils === --

runT  ::            TypeCheckT n m a -> TCState n -> m (a, TCState n)
evalT :: Monad m => TypeCheckT n m a -> TCState n -> m a
execT :: Monad m => TypeCheckT n m a -> TCState n -> m (TCState n)

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: TypeCheck n a -> TCState n -> (a, TCState n)
eval :: TypeCheck n a -> TCState n -> a
exec :: TypeCheck n a -> TCState n -> TCState n

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadTypeCheck n m => (TCState n -> TCState n) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadTypeCheck n m => (TCState n -> (a, TCState n)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadTypeCheck n m => (TCState n -> m (a, TCState n)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadTypeCheck n m => (TCState n -> TCState n) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadTypeCheck n m | m -> n where
    get :: m (TCState n)
    put :: TCState n -> m ()

instance Monad m => MonadTypeCheck n (TypeCheckT n m) where
    get = TypeCheckT   State.get ; {-# INLINE get #-}
    put = TypeCheckT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (TypeCheckT n m) where
    get = TypeCheckT $ lift   State.get ; {-# INLINE get #-}
    put = TypeCheckT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadTypeCheck n m, MonadTrans t, Monad (t m)) => MonadTypeCheck n (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<
