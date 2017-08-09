{-# LANGUAGE RecursiveDo #-}

module Prologue.Control.Monad (module Prologue.Control.Monad, module X) where

import Prelude
import Control.Monad.Fix
import Control.Monad as X ( Monad, (>>=), (>>), (=<<), (<=<), (>=>)
                          , MonadPlus, mplus, mzero
                          , guard, void, join
                          , zipWithM, zipWithM_, foldM, foldM_, forever
                          )

{-# DEPRECATED return "Use `pure` instead" #-}
return :: Applicative m => a -> m a
return = pure ; {-# INLINE return #-}

infixr 1 <<
(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>) ; {-# INLINE (<<) #-}

infixr 1 >>=>, >>>=>, >>>>=>
(>>=>)   :: Monad m => (t1 -> t2             -> m a) -> (a -> m b) -> (t1 -> t2             -> m b)
(>>>=>)  :: Monad m => (t1 -> t2 -> t3       -> m a) -> (a -> m b) -> (t1 -> t2 -> t3       -> m b)
(>>>>=>) :: Monad m => (t1 -> t2 -> t3 -> t4 -> m a) -> (a -> m b) -> (t1 -> t2 -> t3 -> t4 -> m b)
f >>=>   g = \t1 t2       -> f t1 t2       >>= g ; {-# INLINE (>>=>)   #-}
f >>>=>  g = \t1 t2 t3    -> f t1 t2 t3    >>= g ; {-# INLINE (>>>=>)  #-}
f >>>>=> g = \t1 t2 t3 t4 -> f t1 t2 t3 t4 >>= g ; {-# INLINE (>>>>=>) #-}

infixr 1 <=<<, <=<<<, <=<<<<
(<=<<)   :: Monad m => (a -> m b) -> (t1 -> t2             -> m a) -> (t1 -> t2             -> m b)
(<=<<<)  :: Monad m => (a -> m b) -> (t1 -> t2 -> t3       -> m a) -> (t1 -> t2 -> t3       -> m b)
(<=<<<<) :: Monad m => (a -> m b) -> (t1 -> t2 -> t3 -> t4 -> m a) -> (t1 -> t2 -> t3 -> t4 -> m b)
(<=<<)   = flip (>>=>)   ; {-# INLINE (<=<<)   #-}
(<=<<<)  = flip (>>>=>)  ; {-# INLINE (<=<<<)  #-}
(<=<<<<) = flip (>>>>=>) ; {-# INLINE (<=<<<<) #-}

(>>~) :: Monad m => m a -> (a -> m b) -> m a
f >>~ g = do
    fa <- f
    g fa
    pure fa
{-# INLINE (>>~) #-}

infixr 1 =<<&
(=<<&) :: MonadFix m => (a -> m b) -> m a -> m a
g =<<& f = mdo
    g fa
    fa <- f
    pure fa
{-# INLINE (=<<&) #-}
