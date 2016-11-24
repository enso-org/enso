{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Class where

import Luna.Prelude

import           Data.RTuple (List)
import qualified Data.RTuple as List

import qualified Control.Monad.Catch      as Catch
import           Control.Monad.Fix
import qualified Control.Monad.State      as State
import           Control.Monad.State      (StateT)
import           Control.Monad.Primitive

import           Luna.IR.Internal.IR   (Key, IOAccess(..), read, IRMonad, HasIdx)
import           Luna.IR.Term.Layout.Class (Abstract)

import Luna.IR.Layer


-- === Properties === --

type family Keys      pass :: [*]
type family Preserves pass :: [*]


-- === Data declarations ===

type    Pass  pass m   = PassT pass m ()
newtype PassT pass m a = PassT (StateT (State pass) m a)
        deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans, Alternative
                 , MonadFix, Catch.MonadMask
                 , Catch.MonadCatch, Catch.MonadThrow)

type State pass = List (Keys pass)

type        GetKeys  m = Keys  (GetPass m)
type        GetState m = State (GetPass m)
type family GetPass  m where
    GetPass (PassT pass m) = pass
    GetPass (t m)          = GetPass m

makeWrapped ''PassT


-- === Utils ===

eval :: Monad m => PassT pass m a -> List (Keys pass) -> m a
eval = State.evalStateT . unwrap' ; {-# INLINE eval #-}

-- eval :: Pass pass a -> List (Keys pass) -> a
-- eval  = runIdentity .: evalT ; {-# INLINE eval #-}

with :: MonadPass m => (GetState m -> GetState m) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadPass m => (GetState m -> (GetState m, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a
{-# INLINE modify #-}

modify_ :: MonadPass m => (GetState m -> GetState m) -> m ()
modify_ = modify . fmap (,())
{-# INLINE modify_ #-}


-- === MonadPass === --


class Monad m => MonadPass m where
    get :: m (GetState m)
    put :: GetState m -> m ()

instance Monad m => MonadPass (PassT pass m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}

instance {-# OVERLAPPABLE #-} (MonadPass m, MonadTrans t, Monad (t m),GetState m ~ GetState (t m)) => MonadPass (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}


-- === Instances === --

-- Transformats

instance State.MonadState s m => State.MonadState s (PassT pass m) where
    get = wrap' $ lift   State.get ; {-# INLINE get #-}
    put = wrap' . lift . State.put ; {-# INLINE put #-}

-- Primitive

instance PrimMonad m => PrimMonad (PassT pass m) where
    type PrimState (PassT pass m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}


-- <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<

type family KeyAccess key m where
    KeyAccess key (PassT pass m) = FindKey key (Keys pass)
    KeyAccess key (t m)          = KeyAccess key m

type FindKey key lst = Key (FindKeyType key lst) key

type family FindKeyType key lst where
    FindKeyType k (Key s k ': ls) = s
    FindKeyType k (l       ': ls) = FindKeyType k ls


getKey :: forall key m. (MonadPass m, KeyProvider key (GetKeys m)) => m (FindKey key (GetKeys m))
getKey = accessKey <$> get ; {-# INLINE getKey #-}


class KeyProvider key lst where
    accessKey :: List lst -> FindKey key lst

instance {-# OVERLAPPABLE #-} (FindKey k ls ~ FindKey k (l ': ls), KeyProvider k ls)
      => KeyProvider k (l       ': ls) where accessKey = accessKey . view List.tail ; {-# INLINE accessKey #-}
instance KeyProvider k (Key s k ': ls) where accessKey = view List.head ; {-# INLINE accessKey #-}


-- read :: (IRMonad m, HasIdx t)
--      => LayerKey acc el layer -> t -> m (LayerData layer t)
-- read k = readST (unsafeFromKey k) ; {-# INLINE read #-}

read2 :: forall layer t m. (IRMonad m, HasIdx t, MonadPass m, KeyProvider (Layer (Abstract t) layer) (GetKeys m))
      => t -> m (LayerData layer t)
read2 t = flip read t =<< getKey @(Layer (Abstract t) layer)
-- Key () key
-- acc ~ FindKeyType key (GetKeys m)
