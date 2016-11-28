{-# LANGUAGE CPP #-}

module Luna.Pass.Utils.SubtreeWalk where

import Luna.Prelude
import           Data.Construction
import           Old.Data.Prop
import           Data.Record                                     hiding (cons)
import           Old.Data.Graph
import           Old.Data.Graph.Builder
import qualified Old.Data.Graph.Backend.NEC                  as NEC

import           Old.Luna.Syntax.Model.Layer
import           Old.Luna.Syntax.Model.Network.Builder.Layer         (TCDataPayload, seen)
import           Old.Luna.Syntax.Model.Network.Class                 ()


#define PassCtx(m) ( node ~ (ls :<: term)                         \
                   , edge ~ Link node                             \
                   , BiCastable n node                            \
                   , BiCastable e edge                            \
                   , MonadBuilder (Hetero (NEC.Graph n e c)) m    \
                   , Getter Inputs node                           \
                   , Prop Inputs node ~ [Ref Edge edge]           \
                   , HasProp TCData node                          \
                   , Prop TCData node ~ TCDataPayload node        \
                   , ReferencedM Node (Hetero (NEC.Graph n e c)) m (ls :<: term) \
                   , ReferencedM Edge (Hetero (NEC.Graph n e c)) m edge \
                   )

inputs :: PassCtx(m) => Ref Node node -> m [Ref Node node]
inputs = (fmap (# Inputs) . read) >=> mapM (follow source)

resetSeen :: PassCtx(m) => Ref Node node -> m ()
resetSeen ref = do
    withRef ref $ prop TCData . seen .~ False
    inputs ref >>= mapM_ resetSeen

subtreeWalk :: PassCtx(m) => (Ref Node node -> m ()) -> Ref Node node -> m ()
subtreeWalk m ref = do
    seenNode <- follow (prop TCData . seen) ref
    withRef ref $ prop TCData . seen .~ True
    if seenNode then return () else do
        m ref
        inputs ref >>= mapM_ (subtreeWalk m)
