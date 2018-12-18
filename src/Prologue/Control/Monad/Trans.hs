module Prologue.Control.Monad.Trans (module Prologue.Control.Monad.Trans, module X) where

import Prelude
import Control.Monad.Trans.Class as X (MonadTrans, lift)

import Control.Monad.Primitive (PrimState)
import Data.Kind               (Constraint, Type)


-- === Type families === --

type family MonadTranses (ts :: [(Type -> Type) -> Type -> Type]) :: Constraint where
    MonadTranses '[]       = ()
    MonadTranses (t ': ts) = (MonadTrans t, MonadTranses ts)

type MonadTransInvariants     t m = (Monad m, Monad (t m), MonadTrans t)
type PrimMonadTransInvariants t m = (MonadTransInvariants t m, PrimState m ~ PrimState (t m))


-- === Lifting === --

{-# WARNING lift2 "You should not use `lift2` in production code. Use monad transformer stack instead. If you really need it in a very specific use case, use `_lift2_` instead." #-}
lift2, _lift2_ :: (Monad (t1 m), Monad m, MonadTranses '[t1,t2])
               => m a -> t2 (t1 m) a
lift2   = _lift2_     ; {-# INLINE lift2   #-}
_lift2_ = lift . lift ; {-# INLINE _lift2_ #-}

{-# WARNING lift3 "You should not use `lift3` in production code. Use monad transformer stack instead. If you really need it in a very specific use case, use `_lift3_` instead." #-}
lift3, _lift3_ :: (Monad (t2 (t1 m)), Monad (t1 m), Monad m, MonadTranses '[t1,t2,t3])
               => m a -> t3 (t2 (t1 m)) a
lift3   = _lift3_      ; {-# INLINE lift3   #-}
_lift3_ = lift . lift2 ; {-# INLINE _lift3_ #-}

{-# WARNING lift4 "You should not use `lift4` in production code. Use monad transformer stack instead. If you really need it in a very specific use case, use `_lift4_` instead." #-}
lift4, _lift4_ :: (Monad (t3 (t2 (t1 m))), Monad (t2 (t1 m)), Monad (t1 m), Monad m, MonadTranses '[t1,t2,t3,t4])
               => m a -> t4 (t3 (t2 (t1 m))) a
lift4   = _lift4_      ; {-# INLINE lift4   #-}
_lift4_ = lift . lift3 ; {-# INLINE _lift4_ #-}

{-# WARNING lift5 "You should not use `lift5` in production code. Use monad transformer stack instead. If you really need it in a very specific use case, use `_lift5_` instead." #-}
lift5, _lift5_ :: (Monad (t4 (t3 (t2 (t1 m)))), Monad (t3 (t2 (t1 m))), Monad (t2 (t1 m)), Monad (t1 m), Monad m, MonadTranses '[t1,t2,t3,t4,t5])
               => m a -> t5 (t4 (t3 (t2 (t1 m)))) a
lift5   = _lift5_      ; {-# INLINE lift5   #-}
_lift5_ = lift . lift4 ; {-# INLINE _lift5_ #-}
