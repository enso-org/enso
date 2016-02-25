{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Network.Builder.Layer where

import Prologue hiding (read)

import           Data.Graph.Builders
import           Control.Monad.Event
import           Data.Graph.Backend.VectorGraph         (SubGraph)
import           Data.Prop
import           Data.Construction
import           Data.Graph.Backend.VectorGraph
import qualified Luna.Syntax.Model.Network.Builder.Type as Type
import qualified Luna.Syntax.Model.Network.Builder.Self as Self
import           Luna.Syntax.Model.Network.Builder.Self (MonadSelfBuilder, self)
import           Luna.Syntax.AST.Decl.Function          (FunctionPtr)
import           Data.Graph.Builder.Class
import           Luna.Syntax.Model.Layer
import           Data.Graph.Builder.Ref                 as Ref
import           Luna.Syntax.Model.Network.Class
import           Data.Layer.Cover
import           Data.Graph

--------------------------------
-- === Succs registration === --
--------------------------------

-- === Definitions === ---

data SuccRegister = SuccRegister deriving (Show)
instance ( MonadBuilder g m
         , Referred Edge (Arc src tgt) g
         , Referred Node src g
         , Show src
         , Prop Succs src ~ [Ref Edge (Arc src tgt)]
         , HasProp Succs src
         ) => Handler t SuccRegister m (Ref Edge (Arc src tgt)) where
    handler e = do
        ve <- lift $ read e -- FIXME[WD]: remove the lift (it could be handy to disable the magic trans-instance in Graph.hs)
        lift $ Ref.with (ve ^. source) $ prop Succs %~ (e:)
    {-# INLINE handler #-}

instance Monad m => Destructor m (Layer (Network ls) Succs a) where
    destruct _ = return ()

-- === Utils === ---

registerSuccs :: t -> Listener t SuccRegister m a -> m a
registerSuccs _ = unwrap'



------------------------------------------
-- === Native layers implementation === --
------------------------------------------

-- === Succs layer === --

type instance LayerData (Network ls) Succs t = [Ref Edge $ Link (Shelled t)]
instance Monad m => Creator m (Layer (Network ls) Succs a) where create = return $ Layer []


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


-- === Redirect layer === --

type instance LayerData (Network ls) Redirect t = Maybe $ Ref Edge $ Link (Shelled t)
instance Monad m => Creator m (Layer (Network ls) Redirect a) where
    create = return $ Layer Nothing

instance (Monad m, Unregister m (Ref Edge $ Link (Shelled a)))
      => Destructor m (Layer (Network ls) Redirect a) where
    destruct (Layer r) = mapM_ unregister r


-- === Lambda layer === --

type instance LayerData l Lambda (SubGraph n) = Maybe $ FunctionPtr n
instance Monad m => Creator m (Layer l Lambda (SubGraph n)) where
    create = return $ Layer Nothing
instance Monad m => Destructor m (Layer l Lambda (SubGraph n)) where
    destruct _ = return ()

-- === Replacement layer === --

type instance LayerData (Network ls) Replacement t = Maybe $ Ptr Cluster
instance Monad m => Creator m (Layer (Network ls) Replacement a) where
    create = return $ Layer Nothing
instance Monad m => Destructor m (Layer (Network ls) Replacement a) where
    destruct (Layer r) = return ()

------------------------------------------
-- === Layer building & destruction === --
------------------------------------------

instance CoverDestructor m (ls :<: a) => Destructor m (ls :<: a) where destruct a = () <$ destructCover a
