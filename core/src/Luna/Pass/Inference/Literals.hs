{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Pass.Inference.Literals where

import           Luna.Prelude                                    hiding (Num, pre, cons)

import           Data.Construction hiding (cons)
import           Old.Data.Prop
import Old.Data.Record.Match
import           Old.Data.Graph
import           Old.Data.Graph.Builder
import qualified Old.Data.Graph.Backend.NEC                  as NEC

import           Old.Luna.Runtime.Dynamics                         (Static)
import           Old.Luna.Syntax.Term.Class                            hiding (source)
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit
import           Old.Luna.Syntax.Model.Layer
import           Old.Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode, cons)
import           Old.Luna.Syntax.Model.Network.Builder.Term.Class    (NetLayers)
import           Old.Luna.Compilation.Stage.TypeCheck                (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Old.Luna.Compilation.Stage.TypeCheck.Class          (MonadTypeCheck)
import qualified Old.Luna.Compilation.Stage.TypeCheck.Class          as TypeCheck
import           Old.Luna.Syntax.Model.Network.Class                 ()
import           Old.Luna.Syntax.Model.Network.Term


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
