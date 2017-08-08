module Prologue.Control.Monad (module Prologue.Control.Monad, module X) where

import Prelude
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
