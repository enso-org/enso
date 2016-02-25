{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Query.Sort where

import           Control.Monad.Extra        (whenJustM)
import           Control.Monad.Identity
import           Control.Monad.Trans.State
import           Data.Container.Class
import qualified Data.List                  as List
import           Prologue



class (Graph g, ForwardEdgedGraph g, MarkableGraph g, Eq (Item g), Show (Item g)) => CompleteGraph g

class Graph g where
    listNodes :: g -> [Item g]

class BackEdgedGraph g where
    predecessors :: Item g -> g -> [Item g]

class ForwardEdgedGraph g where
    successors :: Item g -> g -> [Item g]

class MarkableGraph g where
    markNode :: Item g -> g -> g
    isMarked :: Item g -> g -> Bool

class SortBy g where
    sortBy :: (Item g -> Bool) -> g -> Either (SortError g) [Item g]

data SortError g = GraphContainsCycle [Item g] [Item g]

deriving instance (Show (Item g)) => Show (SortError g)


data SortState g = SortState
        { _graph  :: g
        , _sorted :: [Item g]
        , _cyclic :: [Item g]
        }

makeLenses ''SortState

instance CompleteGraph g => SortBy g where
    sortBy predicate g = runIdentity $ createResult <$> execStateT sortBy' (SortState g [] []) where
        sortBy' = whenJustM notMarkedNode $ \selected ->
                        visit selected >> sortBy'
        notMarkedNode = List.find predicate <$> (filterNonCyclic . listNodes =<< use graph)
        filterNonCyclic nodes = do
            c <- use cyclic
            return $ List.filter (not . flip elem c) nodes
        visit node = do
            ifM (isMarked node <$> use graph)
                (cyclic %= (node:)) $
                do  graph %= markNode node
                    mapM_ visit . List.filter predicate =<< filterNonCyclic . successors node =<< use graph
                    sorted %= (node:)

        createResult state = if null c
            then Right s
            else Left $ GraphContainsCycle s c
            where s = state ^. sorted
                  c = state ^. cyclic
