{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Text.Location where

import Prelude.Luna

import           Data.Int            (Int64)
import qualified Control.Monad.State as State

---------------------------
-- === Location Data === --
---------------------------

-- === Definitions === --

data Grid = Grid { __line_ :: !Int64, __column_ :: !Int64 } deriving (Show)
makeLenses ''Grid


-- === Accessors === --

class HasLine   a where line   :: Lens' a Int64
class HasColumn a where column :: Lens' a Int64


-- === Instances === --

instance HasLine   Grid where line   = _line_   ; {-# INLINE line   #-}
instance HasColumn Grid where column = _column_ ; {-# INLINE column #-}


-------------------
-- === Delta === --
-------------------

data Delta = Delta !Grid !Grid deriving (Show)




----------------------------
-- === Location Monad === --
----------------------------

---- TODO: template haskellize
---- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Declarations === --

type    Location      = LocationT Identity
newtype LocationT m a = LocationT (State.StateT (Maybe Delta) m a)
                              deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                       , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''LocationT


-- === Utils === --

runT  ::            LocationT m a -> Maybe Delta -> m (a, Maybe Delta)
evalT :: Monad m => LocationT m a -> Maybe Delta -> m a
execT :: Monad m => LocationT m a -> Maybe Delta -> m (Maybe Delta)

runT  = State.runStateT  . unwrap' ; {-# INLINE runT  #-}
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}
execT = State.execStateT . unwrap' ; {-# INLINE execT #-}

run  :: Location a -> Maybe Delta -> (a, Maybe Delta)
eval :: Location a -> Maybe Delta -> a
exec :: Location a -> Maybe Delta -> Maybe Delta

run   = runIdentity .: runT  ; {-# INLINE run  #-}
eval  = runIdentity .: evalT ; {-# INLINE eval #-}
exec  = runIdentity .: execT ; {-# INLINE exec #-}

with :: MonadLocation m => (Maybe Delta -> Maybe Delta) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadLocation m => (Maybe Delta -> (a, Maybe Delta)) -> m a
modify = modifyM . fmap return
{-# INLINE modify #-}

modifyM :: MonadLocation m => (Maybe Delta -> m (a, Maybe Delta)) -> m a
modifyM f = do
    s <- get
    (a, s') <- f s
    put $ s'
    return a
{-# INLINE modifyM #-}

modify_ :: MonadLocation m => (Maybe Delta -> Maybe Delta) -> m ()
modify_ = modify . fmap ((),)
{-# INLINE modify_ #-}


-- === Instances === --

class Monad m => MonadLocation m where
    get :: m (Maybe Delta)
    put :: Maybe Delta -> m ()

instance Monad m => MonadLocation (LocationT m) where
    get = LocationT   State.get ; {-# INLINE get #-}
    put = LocationT . State.put ; {-# INLINE put #-}

instance State.MonadState s m => State.MonadState s (LocationT m) where
    get = LocationT $ lift   State.get ; {-# INLINE get #-}
    put = LocationT . lift . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadLocation m, MonadTrans t, Monad (t m)) => MonadLocation (t m) where
    get = lift get   ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<
