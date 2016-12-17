{-# LANGUAGE UndecidableInstances #-}

module System.Log.Logger.Class where

import Prologue

import           Control.Monad.State          (StateT, runStateT, evalStateT, execStateT)
import qualified Control.Monad.State          as State
import           Control.Monad.Trans.Identity (IdentityT)


--------------------
-- === Logger === --
--------------------

-- === Definition === --

data family Logger l (m :: * -> *) a

class IsLogger l m where
    runLogger :: Logger l m ()
    default runLogger :: Monad (Logger l m) => Logger l m ()
    runLogger = return () ; {-# INLINE runLogger #-}


-- === MonadLogging === --

class Monad m => MonadLogging m where
    runLoggers :: m ()
    default runLoggers :: MonadLoggingTrans t m => t m ()
    runLoggers = lift runLoggers ; {-# INLINE runLoggers #-}

instance {-# OVERLAPPABLE #-} MonadLoggingFound l m
      => MonadLogging (Logger l m) where runLoggers = runLogger >> lift runLoggers ; {-# INLINE runLoggers #-}
instance MonadLogging IO           where runLoggers = return ()                    ; {-# INLINE runLoggers #-}
instance MonadLogging Identity     where runLoggers = return ()                    ; {-# INLINE runLoggers #-}
type MonadLoggingFound l m = (MonadLoggingTrans (Logger l) m, IsLogger l m, MonadLogging m)
type MonadLoggingTrans t m = (Monad m, Monad (t m), MonadTrans t, MonadLogging m)


-- === Instances === --

instance (MonadTrans (Logger l), Monad (Logger l m), PrimMonad m)
      => PrimMonad (Logger l m) where
    type PrimState (Logger l m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}


-- | Standard monad instances
--   We should NOT make a generic overlappable transformer instance here
--   if we want to have any control over GHC type-inferencer errors.

instance MonadLogging m => MonadLogging (StateT s m)
instance MonadLogging m => MonadLogging (IdentityT m)



----------------------------
-- === IdentityLogger === --
----------------------------

-- === Definition === --

data IdentityLogger l
newtype instance Logger (IdentityLogger l) m a = IdentityLogger { fromIdentityLogger :: IdentityT m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadTrans)


-- === Running === --

runIdentityLogger :: Logger (IdentityLogger l) m a -> m a
runIdentityLogger = runIdentityT . fromIdentityLogger ; {-# INLINE runIdentityLogger #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} Monad m => IsLogger (IdentityLogger l) m



-------------------------
-- === StateLogger === --
-------------------------

-- === Definition === --

data StateLogger l
type family StateOf l (m :: * -> *)

type CataStateOf l m = StateOf l (Logger (StateLogger l) m)
newtype instance Logger (StateLogger l) m a = StateLogger { fromStateLogger :: StateT (CataStateOf l m) m a}
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix)


runStateLogger :: forall l m a. Monad m => Logger (StateLogger l) m a -> CataStateOf l m -> m (a, CataStateOf l m)
runStateLogger = runStateT . fromStateLogger ; {-# INLINE runStateLogger #-}

evalStateLogger :: forall l m a. Monad m => Logger (StateLogger l) m a -> CataStateOf l m -> m a
evalStateLogger = evalStateT . fromStateLogger ; {-# INLINE evalStateLogger #-}

execStateLogger :: forall l m a. Monad m => Logger (StateLogger l) m a -> CataStateOf l m -> m (CataStateOf l m)
execStateLogger = execStateT . fromStateLogger ; {-# INLINE execStateLogger #-}


-- === MonadStateLogger === --

class Monad m => MonadLoggerState l m where
    getLoggerState :: m (StateOf l m)
    putLoggerState :: StateOf l m -> m ()

instance Monad m => MonadLoggerState l (Logger (StateLogger l) m) where
    getLoggerState = StateLogger   State.get ; {-# INLINE getLoggerState #-}
    putLoggerState = StateLogger . State.put ; {-# INLINE putLoggerState #-}

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, StateOf l m ~ StateOf l (t m), MonadLoggerState l m)
      => MonadLoggerState l (t m) where
    getLoggerState = lift $ getLoggerState @l ; {-# INLINE getLoggerState #-}
    putLoggerState = lift . putLoggerState @l ; {-# INLINE putLoggerState #-}


-- === Modifications === --

modifyLoggerStateM :: forall l a m. MonadLoggerState l m => (StateOf l m -> m (a, StateOf l m)) -> m a
modifyLoggerStateM f = do
    d <- getLoggerState @l
    (a, d') <- f d
    putLoggerState @l d'
    return a
{-# INLINE modifyLoggerStateM #-}

modifyLoggerStateM_ :: forall l m. MonadLoggerState l m => (StateOf l m -> m (StateOf l m)) -> m ()
modifyLoggerStateM_ = modifyLoggerStateM @l . (fmap.fmap) ((),) ; {-# INLINE modifyLoggerStateM_ #-}

modifyLoggerState :: forall l a m. MonadLoggerState l m => (StateOf l m -> (a, StateOf l m)) -> m a
modifyLoggerState = modifyLoggerStateM @l . fmap return ; {-# INLINE modifyLoggerState #-}

modifyLoggerState_ :: forall l m. MonadLoggerState l m => (StateOf l m -> StateOf l m) -> m ()
modifyLoggerState_ = modifyLoggerStateM_ @l . fmap return ; {-# INLINE modifyLoggerState_ #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-} Monad m => IsLogger (StateLogger l) m

instance MonadTrans (Logger (StateLogger l)) where
    lift = StateLogger . lift ; {-# INLINE lift #-}



---------------------------
-- === Tagged loging === --
---------------------------

-- === Definition === --

-- | MonadTagged is an internal class giving the fine detailed control over tagged functionality.
--   When using `tagged @tag foo`, the following functions fill be executed in order: preTagged *> join (inTagged foo) <* postTagged.
--   Such design gives the ability to cancel `foo` when needed, no matter what the exact type of `foo` is.
class Monad m => MonadTagged tag m where
    preTagged  :: m ()
    postTagged :: m ()
    inTagged   :: forall n. Monad n => n () -> m (n ())

type family MonadTags ts m :: Constraint where
    MonadTags '[]       m = ()
    MonadTags (t ': ts) m = (MonadTagged t m, MonadTags ts m)

instance {-# OVERLAPPABLE #-} (Monad m, Monad (t m), MonadTrans t, MonadTagged tag m)
      => MonadTagged tag (t m) where
    preTagged  = lift $ preTagged  @tag ; {-# INLINE preTagged  #-}
    postTagged = lift $ postTagged @tag ; {-# INLINE postTagged #-}
    inTagged   = lift . inTagged   @tag ; {-# INLINE inTagged   #-}

instance MonadTagged tag IO where
    preTagged  = return () ; {-# INLINE preTagged  #-}
    postTagged = return () ; {-# INLINE postTagged #-}
    inTagged   = return    ; {-# INLINE inTagged   #-}

instance MonadTagged tag Identity where
    preTagged  = return () ; {-# INLINE preTagged  #-}
    postTagged = return () ; {-# INLINE postTagged #-}
    inTagged   = return    ; {-# INLINE inTagged   #-}


-- === Utils === --

tagged :: forall tag m. MonadTagged tag m => m () -> m ()
tagged m = preTagged @tag *> join (inTagged @tag m) <* postTagged @tag ; {-# INLINE tagged #-}
