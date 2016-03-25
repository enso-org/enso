{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Syntax.Model.Network.Builder.Layers.MembershipTracking where

import Prelude.Luna

import           Data.Graph.Builder
import           Control.Monad.Event
import           Data.Graph.Backend.SubGraph          (SubGraph)
import           Data.Prop
import qualified Data.List                               as List
import           Data.Construction
import qualified Data.Graph.Backend.NEC          as VEC
import qualified Data.Graph.Backend.SubGraph as SubGraph
import qualified Luna.Syntax.Model.Network.Builder.Type  as Type
import qualified Luna.Syntax.Model.Network.Builder.Self  as Self
import           Luna.Syntax.Model.Network.Builder.Self  (MonadSelfBuilder, self)
import qualified Luna.Syntax.Term.Function                as Func
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                  as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer.Cover
import           Data.Graph
import           Luna.Syntax.Model.Network.Builder.Layers.TCData

----------------------------------
-- === Member Registration  === --
----------------------------------

data MemberRegister = MemberRegister deriving (Show, Eq)
instance ( MonadBuilder g m
         , Referred Node    g n
         , Referred Cluster g c
         , HasProp TCData n
         , Prop TCData n ~ TCDataPayload n
         ) => Handler t MemberRegister m (SubgraphNodeEvent n c) where
    handler (SubgraphNodeEvent n c) = do
        lift $ withRef n $ prop TCData . belongsTo %~ (cast c :)

registerMembers :: t -> Listener t MemberRegister m a -> m a
registerMembers _ = runListener

-----------------------------
-- === Member Removal  === --
-----------------------------

data MemberRemove = MemberRemove deriving (Show, Eq)
instance ( MonadBuilder g m
         , Referred Node    g n
         , Referred Cluster g c
         , Clusterable n c (Listener t MemberRemove m)
         , g ~ (Hetero (VEC.Graph n' e' (cls :< SubGraph n')))
         , c ~ (cls :< SubGraph n)
         , HasProp TCData n
         , Covered c
         , Prop TCData n ~ TCDataPayload n
         ) => Handler t MemberRemove m (Ref Node n) where
    handler ref = do
        node <- read ref
        let clusters :: [Ref Cluster c] = cast <$> node ^. prop TCData . belongsTo
        mapM_ (exclude ref) clusters

removeMembers :: t -> Listener t MemberRemove m a -> m a
removeMembers _ = runListener

