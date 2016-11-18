{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE GADTs                #-}

module Control.Monad.Event where


import           Prologue

import           Control.Monad.Catch          hiding (Handler)
import           Control.Monad.Fix
import           Control.Monad.State          -- (StateT)
import           Control.Monad.Trans.Identity
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.Delayed   (MonadDelayed, Delayed, delayed)


-------------------
-- === Event === --
-------------------

-- === Definitions === --

class Monad m
   => Event         e m a where dispatch_ :: a -> m ()
type  DelayedEvent  e m a = (Event e (Delayed m) a, MonadDelayed m)
type  Event'          m a = Event        a m a
type  DelayedEvent'   m a = DelayedEvent a m a

type SubEvent     e t m a = (Event e m a, MonadTrans t, Monad (t m))


-- === Dispatching === --

dispatch   :: forall e m a. Event  e m a => a -> m a
dispatch'  :: forall   m a. Event'   m a => a -> m a
dispatch_' :: forall   m a. Event'   m a => a -> m ()
dispatch a = a <$ dispatch_ @e a ; {-# INLINE dispatch   #-}
dispatch'  = dispatch       @a   ; {-# INLINE dispatch'  #-}
dispatch_' = dispatch_      @a   ; {-# INLINE dispatch_' #-}

delayedDispatch_  :: forall e m a. DelayedEvent  e m a => a -> m ()
delayedDispatch   :: forall e m a. DelayedEvent  e m a => a -> m a
delayedDispatch_' :: forall   m a. DelayedEvent'   m a => a -> m ()
delayedDispatch'  :: forall   m a. DelayedEvent'   m a => a -> m a
delayedDispatch_  = delayed . dispatch_  @e   ; {-# INLINE delayedDispatch_  #-}
delayedDispatch a = a <$ delayedDispatch @e a ; {-# INLINE delayedDispatch   #-}
delayedDispatch_' = delayedDispatch_     @a   ; {-# INLINE delayedDispatch_' #-}
delayedDispatch'  = delayedDispatch      @a   ; {-# INLINE delayedDispatch'  #-}

--

redispatch_ :: forall e t m a. SubEvent e t m a => a -> t m ()
redispatch_ = lift ∘ dispatch_ @e ; {-# INLINE redispatch_ #-}



----------------------
-- === Listener === --
----------------------

type    ListenerFunc' ctx m = forall e s. ctx e s m => Proxy e -> s -> m ()
newtype ListenerFunc  ctx m = ListenerFunc (ListenerFunc' ctx m)

type    SingleListener t = Listener (Single t)
type    OneOfListener  t = Listener (OneOf  t)
type    AnyListener      = Listener Any
newtype Listener t ctx m a = Listener (StateT (ListenerFunc ctx m) m a)
        deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadThrow, MonadCatch, MonadMask, MonadPlus, Alternative)


-- === Filters === --

data Any
data Single a
data OneOf  (ls :: [*])


-- === Listening === --

appListenerFunc :: forall e ctx s m. ctx e s m => ListenerFunc ctx m -> s -> m ()
appListenerFunc (ListenerFunc f) = f (Proxy :: Proxy e) ; {-# INLINE appListenerFunc #-}

appListenerFuncT :: forall e ctx s t m. (ctx e s m, Monad m, MonadTrans t) => ListenerFunc ctx m -> s -> t m ()
appListenerFuncT = lift ∘∘ appListenerFunc @e ; {-# INLINE appListenerFuncT #-}


listen       :: forall ev ctx m a. Monad m => ListenerFunc' ctx m -> Listener       ev ctx m a -> m a
listenSingle :: forall ev ctx m a. Monad m => ListenerFunc' ctx m -> SingleListener ev ctx m a -> m a
listenAny    :: forall    ctx m a. Monad m => ListenerFunc' ctx m -> AnyListener       ctx m a -> m a
listen f l   = evalStateT (unwrap' l) (ListenerFunc f) ; {-# INLINE listen       #-}
listenSingle = listen @(Single ev) @ctx                ; {-# INLINE listenSingle #-}
listenAny    = listen @Any @ctx                        ; {-# INLINE listenAny    #-}


-- === Instances === --

-- Wrappers

makeWrapped ''Listener

-- Events

matchedDispatch_ :: forall e t ctx m a. (Event e m a, ctx e a m) => a -> Listener t ctx m ()
matchedDispatch_ a = wrap' (flip (appListenerFuncT @e) a =<< get) *> redispatch_ @e a

instance                                                  Event e Identity                  a where dispatch_ _ = return ()           ; {-# INLINE dispatch_ #-}
instance                                                  Event e IO                        a where dispatch_ _ = return ()           ; {-# INLINE dispatch_ #-}
instance                                                  Event e (ST s)                    a where dispatch_ _ = return ()           ; {-# INLINE dispatch_ #-}
instance {-# OVERLAPPABLE #-}  SubEvent e t m a        => Event e (t m)                     a where dispatch_   = redispatch_      @e ; {-# INLINE dispatch_ #-}
instance {-# OVERLAPPABLE #-}  Event e m a             => Event e (SingleListener e' ctx m) a where dispatch_   = redispatch_      @e ; {-# INLINE dispatch_ #-}
instance {-# OVERLAPPABLE #-} (Event e m a, ctx e a m) => Event e (SingleListener e  ctx m) a where dispatch_   = matchedDispatch_ @e ; {-# INLINE dispatch_ #-}
instance {-# OVERLAPPABLE #-} (Event e m a, ctx e a m) => Event e (AnyListener       ctx m) a where dispatch_   = matchedDispatch_ @e ; {-# INLINE dispatch_ #-}


-- Monads

instance MonadTrans (Listener e ctx) where
    lift = wrap' ∘ lift ; {-# INLINE lift #-}

instance PrimMonad m => PrimMonad (Listener t ctx m) where
    type PrimState (Listener t ctx m) = PrimState m
    primitive = lift ∘ primitive ; {-# INLINE primitive #-}



------------------------
-- === Suppressor === --
------------------------

newtype SuppressorT (t :: Maybe *) m a = SuppressorT (IdentityT m a) deriving (Show, Functor, Monad, MonadTrans, MonadIO, MonadFix, Applicative, MonadThrow, MonadCatch, MonadMask, MonadPlus, Alternative)
type    AllSuppressorT = SuppressorT 'Nothing
makeWrapped ''SuppressorT


-- === Utils === --

runSuppressorT :: SuppressorT t m a -> m a
runSuppressorT = runIdentityT . unwrap' ; {-# INLINE runSuppressorT #-}

suppress :: SuppressorT ('Just t) m a -> m a
suppress = runSuppressorT ; {-# INLINE suppress #-}

suppressAll :: SuppressorT 'Nothing m a -> m a
suppressAll = runSuppressorT ; {-# INLINE suppressAll #-}


-- === Instances === --

-- Events

instance {-# OVERLAPPABLE #-} Monad m     => Event e (SuppressorT 'Nothing   m) a where dispatch_ _ = return ()      ; {-# INLINE dispatch_ #-}
instance {-# OVERLAPPABLE #-} Monad m     => Event e (SuppressorT ('Just e ) m) a where dispatch_ _ = return ()      ; {-# INLINE dispatch_ #-}
instance {-# OVERLAPPABLE #-} Event e m a => Event e (SuppressorT ('Just e') m) a where dispatch_   = redispatch_ @e ; {-# INLINE dispatch_ #-}

-- Monads

instance PrimMonad m => PrimMonad (SuppressorT t m) where
    type PrimState (SuppressorT t m) = PrimState m
    primitive = lift . primitive ; {-# INLINE primitive #-}
