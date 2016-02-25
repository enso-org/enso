{-# LANGUAGE CPP                  #-}

module Luna.Compilation.Pass.Inference.Calling where

import Prelude.Luna
import Data.Construction
import Data.Prop
import Data.Record
import Luna.Evaluation.Runtime                      (Static)
import Luna.Syntax.AST.Decl.Function                (FunctionPtr)
import Luna.Syntax.AST.Term                         hiding (source)
import Data.Graph.Builder                           as Graph hiding (run)
import Data.Graph.Backend.VectorGraph               as Graph
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder            (merge, dupCluster, replacement)
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetGraph, NetLayers, NetCluster)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term
import Type.Inference

import qualified Data.Map as Map
import           Data.Map (Map)

import           Luna.Compilation.Stage.TypeCheck       (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class as TypeCheck

#define PassCtx(m) ( term  ~ Draft Static                         \
                   , ls    ~ NetLayers a                          \
                   , edge  ~ Link (ls :<: term)                   \
                   , node  ~ (ls :<: term)                        \
                   , clus  ~ NetCluster a                         \
                   , graph ~ Hetero (VectorGraph n e c)           \
                   , BiCastable     e edge                        \
                   , BiCastable     n node                        \
                   , BiCastable     c clus                        \
                   , MonadBuilder graph (m)                       \
                   , NodeInferable  (m) (ls :<: term)             \
                   , TermNode Var   (m) (ls :<: term)             \
                   , TermNode Acc   (m) (ls :<: term)             \
                   , TermNode Cons  (m) (ls :<: term)             \
                   , TermNode Lam   (m) (ls :<: term)             \
                   , TermNode Unify (m) (ls :<: term)             \
                   , Referred Node n graph                        \
                   )

{-processNode :: PassCtx => Ref Node node -> m $ Maybe [Ref Node node]-}
{-processNode ref = do-}
    {-node <- read ref-}
    {-caseTest (uncover node) $ do-}
        {-match $ \(App f as) -> do-}
            


