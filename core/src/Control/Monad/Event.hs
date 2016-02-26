{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Event where


import           Prologue

import           Control.Monad.Catch          hiding (Handler)
import           Control.Monad.Fix
import           Control.Monad.State          (StateT)
import           Control.Monad.Trans.Identity


----------------------
-- === Listener === --
----------------------

-- === Definitions === --

class Handler    t cfg m a where handler :: a -> Listener t cfg m ()
newtype Listener t cfg m a = Listener (IdentityT m a) deriving (Show, Functor, Monad, MonadTrans, MonadIO, MonadFix, Applicative, MonadThrow, MonadCatch, MonadMask)
makeWrapped ''Listener

-- Registration time type constraint

instance {-# OVERLAPPABLE #-} (Monad m, Dispatcher t a m)                    => Dispatcher t a (Listener t' cfg m) where dispatch_     = lift ∘∘ dispatch_
instance {-# OVERLAPPABLE #-} (Monad m, Dispatcher t a m, Handler t cfg m a) => Dispatcher t a (Listener t  cfg m) where dispatch_ t a = handler a *> (lift $ dispatch_ t a)


-----------------------------
-- === Type Constraint === --
-----------------------------

-- === Definitions === ---

data TypeConstraint (ctx :: * -> * -> Constraint) tp
instance (ctx a tp, Monad m) => Handler t (TypeConstraint ctx tp) m a where handler _ = return () ; {-# INLINE handler #-}


-- === Constraint rules === ---

class Equality_Full a b
instance a ~ b => Equality_Full a b

class Equality_M1 a b
instance (a ~ ma pa, b ~ mb pb, ma ~ mb) => Equality_M1 a b

class Equality_M2 a b
instance (a ~ m1a (m2a pa), b ~ m1b (m2b pb), m1a ~ m1b, m2a ~ m2b) => Equality_M2 a b

class Equality_M3 a b
instance (a ~ m1a (m2a (m3a pa)), b ~ m1b (m2b (m3b pb)), m1a ~ m1b, m2a ~ m2b, m3a ~ (m3b :: ([*] -> *) -> *)) => Equality_M3 a b
-- FIXME[WD]: remove the kind constraint above


-- === Utils === ---

runListener :: Listener t cfg m a -> m a
runListener = runIdentityT ∘ unwrap'

constrainType :: Proxy ctx -> t -> Proxy tp -> Listener t (TypeConstraint ctx tp) m a -> m a
constrainType _ _ _ = runListener

constrainTypeEq :: t -> Proxy tp -> Listener t (TypeConstraint Equality_Full tp) m a -> m a
constrainTypeM1 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M1   tp) m a -> m a
constrainTypeM2 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M2   tp) m a -> m a
constrainTypeM3 :: t -> Proxy tp -> Listener t (TypeConstraint Equality_M3   tp) m a -> m a
constrainTypeEq = constrainType (p :: P Equality_Full)
constrainTypeM1 = constrainType (p :: P Equality_M1)
constrainTypeM2 = constrainType (p :: P Equality_M2)
constrainTypeM3 = constrainType (p :: P Equality_M3)


----------------------
-- === Dispatcher === --
----------------------
-- | The `dispatch` function can be used to indicate that a particular element is "done".
--   It does not provide any general special meaning. In general, this information can be lost when not used explicitly.
--   For a specific usage look at the `Network` builder, where `dispatch` is used to add type constrains on graph nodes and edges.
--   The `t` parameter is the type of registration, like `Node` or `Edge`. Please keep in mind, that `Node` indicates a "kind" of a structure.
--   It does not equals a graph-like node - it can be a "node" in flat AST representation, like just an ordinary term.


class Monad m => Dispatcher t a m where
    dispatch_ :: t -> a -> m ()


-- === Utils === --

dispatchM :: Dispatcher t a m => t -> m a -> m a
dispatchM t ma = do
    a <- ma
    dispatch_ t a
    return a
{-# INLINE dispatchM #-}

dispatch :: Dispatcher t a m => t -> a -> m a
dispatch t a = a <$ dispatch_ t a ; {-# INLINE dispatch #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-}
         (Dispatcher t a m, MonadTrans f, Monad m, Monad (f m)) => Dispatcher t a (f m)    where dispatch_     = lift ∘∘ dispatch_ ; {-# INLINE dispatch_ #-}
instance                                                         Dispatcher t a IO       where dispatch_ _ _ = return ()         ; {-# INLINE dispatch_ #-}
instance                                                         Dispatcher t a Identity where dispatch_ _ _ = return ()         ; {-# INLINE dispatch_ #-}



