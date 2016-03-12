{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}

module Luna.Compilation.Pass.Utils.Literals where

import           Prelude.Luna                                    hiding (Num, pre)

import           Data.Construction
import           Data.Prop
import Data.Record.Match
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph                  as Graph
import           Type.Inference

import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term                            (Lam, Cons)
import qualified Luna.Syntax.AST.Term.Lit                        as Lit
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term                  (Draft)


#define PassCtx(m, ls, term) ( ls   ~ NetLayers                            \
                             , term ~ Draft Static                         \
                             , ne   ~ Link (ls :<: term)                   \
                             , BiCastable    e ne                          \
                             , BiCastable    n (ls :<: term)               \
                             , MonadIO m                                   \
                             , MonadBuilder (Hetero (VectorGraph n e c)) m \
                             , NodeInferable m (ls :<: term)               \
                             , TermNode Cons m (ls :<: term)               \
                             , TermNode Lam  m (ls :<: term)               \
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
