module Data.Set.Mutable.Class 
  (module Data.Set.Mutable.Class, module X) where

import Prelude
import Data.Item as X


class Monad m => Set m s where
    new        :: m s
    singleton  :: Item s -> m s
    insert     :: s ->  Item s  -> m () 
    insertMany :: s -> [Item s] -> m ()
    delete     :: s ->  Item s  -> m ()
    member     :: s ->  Item s  -> m Bool
    size       :: s -> m Int
    null       :: s -> m Bool
    toList     :: s -> m [Item s]
    fromList   :: [Item s] -> m s

    singleton el = do
        s <- new
        insert s el
        pure s
    {-# INLINE singleton #-}

    insertMany s es = mapM_ (insert s) es ; {-# INLINE insertMany #-}

    null = fmap (== 0) . size ; {-# INLINE null #-} 

    fromList es = do
        s <- new
        insertMany s es
        pure s
    {-# INLINE fromList #-}