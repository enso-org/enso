{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Literals where

import           Prelude.Luna                                    hiding (Num, pre)

import           Data.Construction
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Type.Inference
import           Data.Graph.Builder
import           Data.Graph.Backend.VectorGraph                  as Graph

import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term                            hiding (source)
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


#define PassCtx(m, ls, term) ( ls   ~ NetLayers a                          \
                             , term ~ Draft Static                         \
                             , ne   ~ Link (ls :<: term)                    \
                             , BiCastable    e ne                          \
                             , BiCastable    n (ls :<: term)                \
                             , MonadIO m                                   \
                             , MonadBuilder (Hetero (VectorGraph n e c)) m \
                             , NodeInferable m (ls :<: term)                \
                             , TermNode Cons m (ls :<: term)                \
                             , TermNode Lam  m (ls :<: term)                \
                             )

assignLiteralType :: PassCtx(m, ls, term)
                  => Ref Node (ls :<: term)
                  -> Ref Node (ls :<: term)
                  -> Ref Node (ls :<: term)
                  -> m ()
assignLiteralType consIntRef consStrRef ref = do
    node <- read ref
    caseTest (uncover node) $ do
        let process = void ∘ reconnect ref (prop Type)
        match $ \(Str str) -> process consStrRef
        match $ \(Num num) -> process consIntRef
        match $ \ANY       -> return ()

createLiteralTypes :: PassCtx(m, ls, term) => m (Ref Node (ls :<: term), Ref Node (ls :<: term))
createLiteralTypes = do
    consIntRef <- cons "Int"
    consStrRef <- cons "String"
    return (consIntRef, consStrRef)

runPass :: PassCtx(m, ls, term) => [Ref Node (ls :<: term)] -> m ()
runPass literals = do
    (consIntRef, consStrRef) <- createLiteralTypes
    mapM_ (assignLiteralType consIntRef consStrRef) literals



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

