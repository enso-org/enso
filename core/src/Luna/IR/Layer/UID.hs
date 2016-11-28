{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Layer.UID where

import Data.Data (Data)

import Luna.Prelude
import Luna.IR.Layer.Class

import           Control.Monad.State (StateT)
import qualified Control.Monad.State as State



------------------------------
-- === Type definitions === --
------------------------------

newtype ID = ID Word64 deriving (Bounded, Default, Enum, Eq, Integral, Data, Num, Ord, Read, Real, Show)


----------------------
-- === MonadUID === --
----------------------

newtype UIDStoreT m a = UIDStoreT (StateT ID m a) deriving ( Functor, Monad, Applicative, MonadIO, MonadPlus, MonadTrans
                                                           , Alternative, MonadFix, MonadMask, MonadCatch, MonadThrow)

makeWrapped ''UIDStoreT

class Monad m => MonadUID m where
    get :: m ID
    put :: ID -> m ()

instance {-# OVERLAPPABLE #-} (MonadUID m, Monad (t m), MonadTrans t) => MonadUID (t m) where
    get = lift   get ; {-# INLINE get #-}
    put = lift . put ; {-# INLINE put #-}

instance Monad m => MonadUID (UIDStoreT m) where
    get = wrap'   State.get ; {-# INLINE get #-}
    put = wrap' . State.put ; {-# INLINE put #-}


evalT :: Monad m => UIDStoreT m a -> ID -> m a
evalT = State.evalStateT . unwrap' ; {-# INLINE evalT #-}

with :: MonadUID m => (ID -> ID) -> m a -> m a
with f m = do
    s <- get
    put $ f s
    out <- m
    put s
    return out
{-# INLINE with #-}

modify :: MonadUID m => (ID -> (ID, a)) -> m a
modify f = do
    s <- get
    let (s', a) = f s
    put $ s'
    return a
{-# INLINE modify #-}

modify_ :: MonadUID m => (ID -> ID) -> m ()
modify_ = modify . fmap (,())
{-# INLINE modify_ #-}


-- === Instances === --

instance PrimMonad m => PrimMonad (UIDStoreT m) where
    type PrimState (UIDStoreT m) = PrimState m
    primitive = lift . primitive
    {-# INLINE primitive #-}


--- === Utils === --

evalNewT :: Monad m => UIDStoreT m a -> m a
evalNewT = flip evalT def ; {-# INLINE evalNewT #-}

genUID :: MonadUID m => m ID
genUID = get <* modify_ succ ; {-# INLINE genUID #-}



-----------------------
-- === UID layer === --
-----------------------


data UID = UID deriving (Show)

type instance LayerData UID t = ID

instance MonadUID m => LayerCons UID m where
    consLayer _ _ = Layer <$> genUID ; {-# INLINE consLayer #-}
