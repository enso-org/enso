{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Utils.Literals where

import           Prelude.Luna                                    hiding (Num, pre)

import           Data.Construction
import           Data.Prop
import Data.Record.Match
import           Data.Graph
import           Data.Graph.Builder
import qualified Data.Graph.Backend.NEC                  as NEC
import           Type.Inference

import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                            (Lam, Cons)
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term                  (Draft)
import           Data.Layer_OLD.Cover_OLD


#define PassCtx(m, ls, term) ( ls    ~ NetLayers                \
                             , term  ~ Draft Static             \
                             , node  ~ (ls :<: term)            \
                             , edge  ~ Link node                \
                             , graph ~ Hetero (NEC.Graph n e c) \
                             , BiCastable    e edge             \
                             , BiCastable    n node             \
                             , MonadIO m                        \
                             , MonadBuilder graph m             \
                             , NodeInferable m node             \
                             , TermNode Cons m node             \
                             , TermNode Lam  m node             \
                             , ReferencedM Node graph (m) node  \
                             , ReferencedM Edge graph (m) edge  \
                             )

pre :: PassCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
pre ref = mapM (follow source) =<< (# Inputs) <$> read ref

collectLiterals :: PassCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
collectLiterals ref = do
    prev  <- pre ref
    lists <- mapM collectLiterals prev
    node  <- read ref
    return $ caseTest (uncover node) $ do
        let list = join lists
        of' $ \(Lit.String {}) -> ref : list
        of' $ \(Lit.Number {}) -> ref : list
        of' $ \ANY             -> list

run :: PassCtx(m, ls, term) => Ref Node (ls :<: term) -> m [Ref Node (ls :<: term)]
run root = collectLiterals root
