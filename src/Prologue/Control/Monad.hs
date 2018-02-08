{-# LANGUAGE RecursiveDo #-}

module Prologue.Control.Monad (module Prologue.Control.Monad, module X) where

import Prelude hiding (return, mempty)
import Control.Monad.Fix
import Control.Monad as X ( Monad, (>>=), (>>), (=<<), (<=<), (>=>)
                          , MonadPlus, mplus, mzero
                          , join
                          , zipWithM, zipWithM_, foldM, foldM_, forever
                          )
import Data.Convert
import Data.Monoids
import Prologue.Data.Basic
import Unsafe.Coerce (unsafeCoerce)


{-# DEPRECATED return "Use `pure` instead" #-}
return :: Applicative m => a -> m a
return = pure ; {-# INLINE return #-}

infixr 1 <<
(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>) ; {-# INLINE (<<) #-}

infixr 0 >>$
(>>$) :: Monad m => m a -> m b -> m b
(>>$) = (>>) ; {-# INLINE (>>$) #-}

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
    _  <- g fa
    pure fa
{-# INLINE (>>~) #-}

infixr 1 =<<&
(=<<&) :: MonadFix m => (a -> m b) -> m a -> m a
g =<<& f = mdo
    _  <- g fa
    fa <- f
    pure fa
{-# INLINE (=<<&) #-}

-- The void function implemented with Functor / Applicative or Monad is slow.
-- GHC cannot optimize it, especially when using in IO monad, it could slow
-- down function even more than 10 times (!).
void :: m a -> m ()
void = unsafeCoerce ; {-# INLINE void #-}

when  , unless   :: (Applicative m, Mempty a) =>   Bool -> m a -> m a
when_ , unless_  :: (Applicative m)           =>   Bool -> m a -> m ()
whenM , unlessM  :: (Monad m      , Mempty a) => m Bool -> m a -> m a
whenM_, unlessM_ :: (Monad m      )           => m Bool -> m a -> m ()
when     p s = ifThenElse p s             (pure mempty) ; {-# INLINE when     #-}
unless   p s = ifThenElse p (pure mempty) s             ; {-# INLINE unless   #-}
when_    p s = ifThenElse p (void s)      (pure ())     ; {-# INLINE when_    #-}
unless_  p s = ifThenElse p (pure ())     (void s)      ; {-# INLINE unless_  #-}
whenM    p s = flip when    s =<< p                     ; {-# INLINE whenM    #-}
unlessM  p s = flip unless  s =<< p                     ; {-# INLINE unlessM  #-}
whenM_   p s = flip when_   s =<< p                     ; {-# INLINE whenM_   #-}
unlessM_ p s = flip unless_ s =<< p                     ; {-# INLINE unlessM_ #-}

guard :: (MonadPlus m, ToBool' cond) => cond -> m ()
guard cond = case toBool' cond of
    True  -> return ()
    False -> mzero
{-# INLINE guard #-}
