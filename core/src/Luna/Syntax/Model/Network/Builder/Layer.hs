{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layer (
      module Luna.Syntax.Model.Network.Builder.Layer
    , module X
    ) where

import Prelude.Luna

import Luna.Syntax.Model.Network.Builder.Layers.SuccTracking as X

import           Data.Graph.Builders
import           Control.Monad.Event
import           Data.Graph.Backend.VectorGraph         (SubGraph)
import           Data.Prop
import           Data.Construction
import           Data.Graph.Backend.VectorGraph
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder, self)
import qualified Luna.Syntax.AST.Function               as Func
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                 as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer.Cover
import           Data.Graph

------------------------------------------
-- === Native layers implementation === --
------------------------------------------

-- === Type layer === --

type instance LayerData (Network ls) Type t = Ref Edge $ Link (Shelled t)

instance (MonadSelfBuilder s m, (Link l) ~ Connection s (Ref Node l), Connectible s (Ref Node l) m, l ~ Shelled a)
      => Creator m (Layer (Network ls) Type a) where
    create = Layer <$> do
        s <- self
        let tgt = Ref 0 :: Ref Node l -- FIXME[WD]: Pure magic. 0 is the ID of Star
        connection tgt s

instance (Monad m, Destructor m (LayerData (Network ls) Type a)) => Destructor m (Layer (Network ls) Type a) where
    destruct (Layer ref) = destruct ref

-- === TCData layer === --

data TCDataPayload n = TCDataPayload { _redirect    :: Maybe $ Ref Edge $ Link n
                                     , _replacement :: Maybe $ Ptr Cluster
                                     , _belongsTo   :: [Ptr Cluster]
                                     , _keep        :: Bool
                                     , _seen        :: Bool
                                     } deriving (Show, Eq)
makeLenses ''TCDataPayload

type instance LayerData (Network ls) TCData t = TCDataPayload (Shelled t)
instance Monad m => Creator m (Layer (Network ls) TCData a) where
    create = return $ Layer $ TCDataPayload Nothing Nothing [] False False

instance (Monad m, Unregister m (Ref Edge $ Link (Shelled a)))
      => Destructor m (Layer (Network ls) TCData a) where
    destruct (Layer (TCDataPayload red _ _ _ _)) = mapM_ unregister red

instance Castable t t' => Castable (TCDataPayload t) (TCDataPayload t') where
    cast (TCDataPayload a b c d e) = TCDataPayload (cast <$> a) b c d e

-- === Lambda layer === --

type instance LayerData l Lambda (SubGraph n) = Maybe $ Func.Signature (Ref Node n)
instance Monad m => Creator m (Layer l Lambda (SubGraph n)) where
    create = return $ Layer Nothing
instance Monad m => Destructor m (Layer l Lambda (SubGraph n)) where
    destruct _ = return ()

------------------------------------------
-- === Layer building & destruction === --
------------------------------------------

instance CoverDestructor m (ls :<: a) => Destructor m (ls :<: a) where destruct a = () <$ destructCover a

---------------------------------------
-- === Cluster member management === --
---------------------------------------

-- === Definitions === ---

data MemberRegister = MemberRegister deriving (Show, Eq)
instance ( MonadBuilder g m
         , Referred Node    n g
         , Referred Cluster c g
         , HasProp TCData n
         , Prop TCData n ~ TCDataPayload n
         ) => Handler t MemberRegister m (SubgraphNodeEvent n c) where
    handler (SubgraphNodeEvent n c) = do
        lift $ withRef n $ prop TCData . belongsTo %~ (cast c :)

-- === Utils === ---

registerMembers :: t -> Listener t MemberRegister m a -> m a
registerMembers _ = runListener

