module Luna.Prelude (module Luna.Prelude, module X) where

import           Data.Reprx        as X
import           Prologue_old          as X hiding (Type, Curry, Cons, Getter, Repr, Setter, cons, read, repr, (#), universe)
import           Data.Construction as X hiding (Fields, Product, Product', fields, fields', product)
import           Control.Monad     as X (forM, forM_, filterM)
import           Data.Maybe        as X (maybeToList, maybe, fromMaybe)
import           Unique            as X (Uniquable, getUnique)
import           Data.Binary       as X (Binary)
import           Data.List         (dropWhileEnd)
import           Data.Char         (isSpace)

import           Control.Monad.Except (throwError, MonadError)

infixr 1 <?!>
(<?!>) :: MonadError e m => m (Maybe a) -> e -> m a
a <?!> e = a >>= maybe (throwError e) return

normalizeQQ :: String -> String
normalizeQQ str = intercalate "\n" $ fmap (drop minWs) allLines where
    allLines = filter (not . null) $ dropWhileEnd isSpace <$> lines str
    minWs    = minimum $ length . takeWhile isSpace <$> allLines


partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM pred l = partitionM' ([], []) pred $ reverse l where
    partitionM' a         _ [] = return a
    partitionM' ~(ts, fs) p (x : xs) = do
        res <- p x
        if res then partitionM' (x : ts, fs) p xs else partitionM' (ts, x : fs) p xs

scanM :: Monad m => (a -> b -> m a) -> a -> [b] -> m [a]
scanM f a [] = return [a]
scanM f a (e : es) = do
    current <- f a e
    rest    <- scanM f current es
    return (a : rest)
