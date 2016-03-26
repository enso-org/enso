{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE RecursiveDo            #-}

module Luna.Syntax.Model.Network.Builder.Self where

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.Primitive
import           Prelude.Luna
import qualified Language.Haskell.Session as HS

-- TODO: template haskellize
-- >->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->->

-- === Data declarations ===

#define STATE_DEF(s,m,a) (State.StateT s m a)

type    SelfBuilder  s     = SelfBuilderT s Identity
newtype SelfBuilderT s m a = SelfBuilderT STATE_DEF(s,m,a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative
                 , MonadFix, HS.GhcMonad, HS.ExceptionMonad, HS.HasDynFlags, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

class MonadFix m => MonadSelfBuilder s m | m -> s where
    get :: m s
    put :: s -> m ()


-- === Utils ===

runT  ::            SelfBuilderT s m a -> s -> m (a, s)
evalT :: Monad m => SelfBuilderT s m a -> s -> m a
execT :: Monad m => SelfBuilderT s m a -> s -> m s

runT  = State.runStateT  . view _Wrapped'
evalT = State.evalStateT . view _Wrapped'
execT = State.execStateT . view _Wrapped'


run  :: SelfBuilder s a -> s -> (a, s)
eval :: SelfBuilder s a -> s -> a
exec :: SelfBuilder s a -> s -> s

run   = runIdentity .: runT
eval  = runIdentity .: evalT
exec  = runIdentity .: execT

with :: MonadSelfBuilder s m => (s -> s) -> m b -> m b
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out

modify :: MonadSelfBuilder s m => (s -> (s, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a

modify_ :: MonadSelfBuilder s m => (s -> s) -> m ()
modify_ = modify . fmap (,())


-- === Instances === --

-- Wrappers

instance Rewrapped (SelfBuilderT s m a) (SelfBuilderT s' m' a')
instance Wrapped   (SelfBuilderT s m a) where
    type Unwrapped (SelfBuilderT s m a) = STATE_DEF(s,m,a)
    _Wrapped' = iso (\(SelfBuilderT s) -> s) SelfBuilderT

-- Basic monads

instance MonadFix m => MonadSelfBuilder s (SelfBuilderT s m) where
    get = SelfBuilderT State.get
    put = SelfBuilderT . State.put

instance State.MonadState s m => State.MonadState s (SelfBuilderT s m) where
    get = SelfBuilderT (lift State.get)
    put = SelfBuilderT . lift . State.put

instance {-# OVERLAPPABLE #-} (MonadSelfBuilder s m, MonadTrans t, MonadFix (t m)) => MonadSelfBuilder s (t m) where
    get = lift get
    put = lift . put

-- Primitive
instance PrimMonad m => PrimMonad (SelfBuilderT s m) where
    type PrimState (SelfBuilderT s m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}


-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

self :: MonadSelfBuilder s m => m s
self = get

setSelf :: MonadSelfBuilder s m => s -> m ()
setSelf = put

buildMe :: MonadSelfBuilder a m => m a -> m a
buildMe f = mdo
    setSelf me
    me <- f
    return me

buildAbsMe :: (Castable a s, MonadSelfBuilder s m) => m a -> m a
buildAbsMe f = mdo
    setSelf (cast me)
    me <- f
    return me

-- FIXME[WD]: refactor the name and use it while constructing graphs instead of buildMe and buildAbsMe
buildMe2 :: MonadFix m => SelfBuilderT a m a -> m a
buildMe2 = mfix ∘ evalT

-- <=>

--buildMe2 f = mdo
--    me <- evalT f me
--    return me
