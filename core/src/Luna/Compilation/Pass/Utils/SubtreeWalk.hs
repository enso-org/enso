{-# LANGUAGE CPP #-}

module Luna.Compilation.Pass.Utils.SubtreeWalk where

import Prelude.Luna
import           Data.Construction
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph                  as Graph

import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Layer         (TCDataPayload, seen)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Class                 ()


#define PassCtx(m) ( node ~ (ls :<: term)                         \
                   , edge ~ Link node                             \
                   , BiCastable n node                            \
                   , BiCastable e edge                            \
                   , MonadBuilder (Hetero (VectorGraph n e c)) m  \
                   , Getter Inputs node                           \
                   , Prop Inputs node ~ [Ref Edge edge]           \
                   , HasProp TCData node                          \
                   , Prop TCData node ~ TCDataPayload node        \
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
