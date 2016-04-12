module Prelude.Luna (module Prelude.Luna, module X) where

import           Data.Reprx        as X
import           Prologue          as X hiding (Curry, Cons, Getter, Repr, Setter, cons, read, repr, (#), universe)
import           Data.Construction as X
import           Control.Monad     as X (forM, forM_, filterM)
import           Data.Maybe        as X (maybeToList, maybe, fromMaybe)

import           Control.Monad.Except (throwError, MonadError)

infixr 1 <?!>
(<?!>) :: MonadError e m => m (Maybe a) -> e -> m a
a <?!> e = a >>= maybe (throwError e) return

partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred l = partitionM' ([], []) pred $ reverse l where
    partitionM' a         _ [] = return a
    partitionM' ~(ts, fs) p (x : xs) = do
        res <- p x
        if res then partitionM' (x : ts, fs) p xs else partitionM' (ts, x : fs) p xs
