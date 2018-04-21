module Luna.IR.Component.Term.Inputs where

import Prologue

import Luna.IR (SomeLink)


--------------------
-- === Inputs === --
--------------------

-- === Definition === --

class HasInputs (a :: Type -> Type) where
    inputsIO :: ∀ t. a t -> IO [SomeLink]


-- === API ===--

inputs :: (HasInputs a, MonadIO m) => a t -> m [SomeLink]
inputs = liftIO . inputsIO ; {-# INLINE inputs #-}


-- === Discovery === --
