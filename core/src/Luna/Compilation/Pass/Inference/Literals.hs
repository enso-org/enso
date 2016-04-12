{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Literals where

import           Prelude.Luna                                    hiding (Num, pre)

import           Data.Construction
import           Data.Prop
import Data.Record.Match
import           Type.Inference
import           Data.Graph
import           Data.Graph.Builder
import qualified Data.Graph.Backend.NEC                  as NEC

import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                            hiding (source)
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Compilation.Stage.TypeCheck                (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class          (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheck
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term


import           Luna.Syntax.Term.Function.Argument (Arg)
import           Data.Layer_OLD.Cover_OLD


#define PassCtx(m, ls, term) ( ls    ~ NetLayers                 \
                             , term  ~ Draft Static              \
                             , node  ~ (ls :<: term)             \
                             , edge  ~ Link node                 \
                             , graph ~ Hetero (NEC.Graph n e c)  \
                             , BiCastable    e edge              \
                             , BiCastable    n node              \
                             , MonadIO       (m)                 \
                             , MonadBuilder  graph (m)           \
                             , NodeInferable (m) node            \
                             , TermNode Cons (m) node            \
                             , TermNode Lam  (m) node            \
                             , Destructor    (m) (Ref Edge edge) \
                             , ReferencedM Node graph (m) node   \
                             , ReferencedM Edge graph (m) edge   \
                             )

assignLiteralType :: PassCtx(m, ls, term) => Ref Node (ls :<: term) -> m ()
assignLiteralType ref = do
    node <- read ref
    caseTest (uncover node) $ do
        let process = void . (reconnect (prop Type) ref <=< (flip cons []))
        of' $ \(Lit.String {} ) -> process "String"
        of' $ \(Lit.Number _ n) -> case n of
            Lit.Integer _ -> process "Int"
            Lit.Double  _ -> process "Double"
        of' $ \(Cons (Lit.String n) as) -> case n of
            -- FIXME[MK]: Hardcoded Bool types. We need a dynamic mapping between constructors and their types, probably managed in a separate pass
            "True"  -> process "Bool"
            "False" -> process "Bool"
            _       -> return ()
        of' $ \ANY             -> return ()

runPass :: PassCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
runPass literals = do
    mapM_ assignLiteralType literals


-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data LiteralsPass = LiteralsPass deriving (Show, Eq)

instance ( PassCtx(m, ls, term)
         , MonadTypeCheck (ls :<: term) m
         ) => TypeCheckerPass LiteralsPass m where
    hasJobs _ = not . null . view TypeCheck.untypedLits <$> TypeCheck.get

    runTCPass _ = do
        lits <- view TypeCheck.untypedLits <$> TypeCheck.get
        TypeCheck.modify_ $ TypeCheck.untypedLits .~ []
        runPass lits
        case lits of
            [] -> return Stuck
            _  -> return Progressed
