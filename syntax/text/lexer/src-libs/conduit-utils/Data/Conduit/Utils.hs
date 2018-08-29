{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}

module Data.Conduit.Utils where

import Prelude

import           Data.Conduit


---------------------------
-- === Conduit utils === --
---------------------------

awaitJust :: Monad m => (a -> ConduitM a o m ()) -> ConduitM a o m ()
awaitJust f = await >>= maybe (return ()) f ; {-# INLINE awaitJust #-}

withPeek :: Monad m => (a -> ConduitM a o m ()) -> ConduitM a o m ()
withPeek f = awaitJust $ \x -> leftover x >> f x ; {-# INLINE withPeek #-}

whenNonEmpty :: Monad m => ConduitM a o m () -> ConduitM a o m ()
whenNonEmpty = withPeek . const ; {-# INLINE whenNonEmpty #-}

useRight :: Monad m => (t -> ConduitM i (Either a b) m ()) -> Either a t -> ConduitM i (Either a b) m ()
useRight f = \case
    Left  e -> yield $ Left e
    Right v -> f v
{-# INLINE useRight #-}
