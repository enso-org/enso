{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Literals where

import           Prelude.Luna                                    hiding (Num, pre)

import           Data.Construction
import           Data.Prop
import Data.Record.Match
import           Type.Inference
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph                  as Graph

import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Luna.Syntax.Term.Expr                            hiding (source)
import qualified Luna.Syntax.Term.Lit                        as Lit
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

#define PassCtx(m, ls, term) ( ls   ~ NetLayers                               \
                             , term ~ Draft Static                            \
                             , ne   ~ Link (ls :<: term)                      \
                             , node ~ (ls :<: term)                           \
                             , BiCastable    e ne                             \
                             , BiCastable    n (ls :<: term)                  \
                             , MonadIO       (m)                              \
                             , MonadBuilder  (Hetero (VectorGraph n e c)) (m) \
                             , NodeInferable (m) (ls :<: term)                \
                             , TermNode Cons (m) (ls :<: term)                \
                             , TermNode Lam  (m) (ls :<: term)                \
                             , Destructor    (m) (Ref Edge ne)                \
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

