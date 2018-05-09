module Data.Graph.Traversal.Discovery where

import Prologue

import qualified Control.Monad.State.Layered       as State
import qualified Data.Graph.Data.Component.Class   as Component
import qualified Data.Graph.Data.Component.Dynamic as Component
import qualified Data.Graph.Data.Component.Dynamic as Dynamic
import qualified Data.Map.Strict                   as Map
import qualified Data.Set                          as Set

import Data.Graph.Data.Component.Class    (Component)
import Data.Graph.Data.Component.Provider (DynamicTraversalMap (..))
import Data.Set                           (Set)



---------------------------------
-- === Component Discovery === --
---------------------------------

-- === API === --

discover :: (MonadIO m, State.Getter DynamicTraversalMap m, Typeable tag)
    => Component tag layout -> m (Set Component.Dynamic)
discover = discoverDynamic . Component.toDynamic1
{-# INLINE discover #-}

discoverDynamic :: (MonadIO m, State.Getter DynamicTraversalMap m)
    => Component.Dynamic -> m (Set Component.Dynamic)
discoverDynamic !comp = do
    !info <- State.get @DynamicTraversalMap
    discoverDynamic__ info mempty comp
{-# INLINE discoverDynamic #-}

discoverDynamic__ :: MonadIO m
    => DynamicTraversalMap -> Set Component.Dynamic -> Component.Dynamic
    -> m (Set Component.Dynamic)
discoverDynamic__ !info = go where
    go !all !comp = do
        !nbrs <- getNeighbours info comp
        let !newComps = filter (flip Set.notMember all) nbrs
            !all'     = foldr Set.insert all newComps
        foldM go all' newComps
{-# INLINE discoverDynamic__ #-}

getNeighbours :: MonadIO m
    => DynamicTraversalMap -> Component.Dynamic -> m [Component.Dynamic]
getNeighbours info comp = neighbours where
    Component.Rep tagRep !_ = comp ^. Dynamic.rep
    !compPtr    = comp ^. Dynamic.ptr
    !compInfo   = unsafeFromJust $ Map.lookup tagRep $ unwrap info  -- TODO: convert to panic
    !neighbours = liftIO $ compInfo compPtr
{-# INLINE getNeighbours #-}
