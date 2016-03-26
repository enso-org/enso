{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}

module Luna.Syntax.Model.Network.Builder.Type where

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.Primitive
import           Prelude.Luna
import qualified Language.Haskell.Session as HS

-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Data declarations ===

#define STATE_DEF(s,m,a) (State.StateT (Maybe s) m a)

type    TypeBuilder  s     = TypeBuilderT s Identity
newtype TypeBuilderT s m a = TypeBuilderT STATE_DEF(s,m,a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative
                 , MonadFix, HS.GhcMonad, HS.ExceptionMonad, HS.HasDynFlags, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

class Monad m => MonadTypeBuilder s m | m -> s where
    get :: m (Maybe s)
    put :: Maybe s -> m ()


-- === Utils ===

runT  ::            TypeBuilderT s m a -> Maybe s -> m (a, Maybe s)
evalT :: Monad m => TypeBuilderT s m a -> Maybe s -> m a
execT :: Monad m => TypeBuilderT s m a -> Maybe s -> m (Maybe s)

runT  = State.runStateT  . view _Wrapped'
evalT = State.evalStateT . view _Wrapped'
execT = State.execStateT . view _Wrapped'


run  :: TypeBuilder s a -> Maybe s -> (a, Maybe s)
eval :: TypeBuilder s a -> Maybe s -> a
exec :: TypeBuilder s a -> Maybe s -> Maybe s

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: MonadTypeBuilder s m => (Maybe s -> Maybe s) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: MonadTypeBuilder s m => (Maybe s -> (Maybe s, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a

modify_ :: MonadTypeBuilder s m => (Maybe s -> Maybe s) -> m ()
modify_ = modify . fmap (,())


-- === Instances === --

-- Wrappers

instance Rewrapped (TypeBuilderT s m a) (TypeBuilderT s' m' a')
instance Wrapped   (TypeBuilderT s m a) where
    type Unwrapped (TypeBuilderT s m a) = STATE_DEF(s,m,a)
    _Wrapped' = iso (\(TypeBuilderT s) -> s) TypeBuilderT

-- Basic monads

instance Monad m => MonadTypeBuilder s (TypeBuilderT s m) where
    get = TypeBuilderT State.get
    put = TypeBuilderT . State.put

instance State.MonadState s m => State.MonadState s (TypeBuilderT s m) where
    get = TypeBuilderT (lift State.get)
    put = TypeBuilderT . lift . State.put

instance {-# OVERLAPPABLE #-} (MonadTypeBuilder s m, MonadTrans t, Monad (t m)) => MonadTypeBuilder s (t m) where
    get = lift get
    put = lift . put

-- Primitive
instance PrimMonad m => PrimMonad (TypeBuilderT s m) where
    type PrimState (TypeBuilderT s m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}

-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

ask :: MonadTypeBuilder s m => m (Maybe s)
ask = get

set :: MonadTypeBuilder s m => s -> m ()
set = put . Just
