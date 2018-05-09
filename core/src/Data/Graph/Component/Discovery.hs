module Data.Graph.Component.Discovery where

import Prologue

import qualified Control.Monad.State.Layered   as State
import qualified Data.Graph.Component.Class    as Component
import qualified Data.Graph.Component.Dynamic  as Component
import qualified Data.Graph.Component.Dynamic  as Dynamic
import qualified Data.Graph.Component.Provider as Component
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified OCI.Pass.State.IRInfo         as IRInfo

import Data.Graph.Component.Class (Component)
import Data.Set                   (Set)
import OCI.Pass.State.IRInfo      (CompiledIRInfo)



discover :: (MonadIO m, State.Getter CompiledIRInfo m, Typeable tag)
    => Component tag layout -> m (Set Component.Dynamic)
discover = discoverDynamic . Component.toDynamic1
{-# INLINE discover #-}

discoverDynamic :: (MonadIO m, State.Getter CompiledIRInfo m)
    => Component.Dynamic -> m (Set Component.Dynamic)
discoverDynamic comp = do
    info <- State.get @CompiledIRInfo
    discoverDynamic__ info mempty comp
{-# INLINE discoverDynamic #-}

discoverDynamic__ :: MonadIO m
    => CompiledIRInfo -> Set Component.Dynamic -> Component.Dynamic
    -> m (Set Component.Dynamic)
discoverDynamic__ info = go where
    go all comp = do
        nbrs <- getNeighbours info comp
        let newComps = filter (flip Set.notMember all) nbrs
            all'     = foldr Set.insert all newComps
        foldM go all' newComps
{-# INLINE discoverDynamic__ #-}

getNeighbours :: MonadIO m
    => CompiledIRInfo -> Component.Dynamic -> m [Component.Dynamic]
getNeighbours info comp = neighbours where
    Component.Rep tagRep _ = comp ^. Dynamic.rep
    compPtr    = comp ^. Dynamic.ptr
    compInfo   = unsafeFromJust $ Map.lookup tagRep
               $ info ^. IRInfo.compiledComponents -- TODO: convert to panic
    neighbours = liftIO $ (compInfo ^. IRInfo.layersComponents) compPtr
{-# INLINE getNeighbours #-}
