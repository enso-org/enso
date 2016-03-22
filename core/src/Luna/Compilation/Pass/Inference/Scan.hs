{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Scan where
import Prelude.Luna
import Data.Construction
import Data.Prop
import Data.Record.Match
import Data.Graph.Builder
import Data.Graph.Backend.VectorGraph                  as Graph

import Luna.Syntax.Term.Expr                            hiding (source, target)
import Luna.Syntax.Model.Layer
import Luna.Runtime.Dynamics                         (Static)
import Luna.Syntax.Model.Network.Class                 ()
import Luna.Syntax.Model.Network.Builder.Node.Inferred
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Term

import Luna.Compilation.Pass.Utils.SubtreeWalk         (subtreeWalk)
import Luna.Syntax.Model.Network.Builder.Layer         (TCDataPayload)

import           Luna.Compilation.Stage.TypeCheck                (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class          (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheck
import qualified Luna.Syntax.Term.Lit                        as Lit

#define PassCtx(m) ( term ~ Draft Static                          \
                   , Covered node                                 \
                   , node ~ (ls :<: term)                         \
                   , edge ~ Link node                             \
                   , BiCastable n node                            \
                   , BiCastable e edge                            \
                   , MonadBuilder (Hetero (VectorGraph n e c)) m  \
                   , NodeInferable  (m) node                      \
                   , Getter Inputs node                           \
                   , Prop Inputs node ~ [Ref Edge edge]           \
                   , HasProp TCData node                          \
                   , Prop TCData node ~ TCDataPayload node        \
                   , MonadTypeCheck (ls :<: term) m               \
                   )

investigate :: PassCtx(m) => Ref Node node -> m ()
investigate ref = do
    node <- read ref
    let mod = caseTest (uncover node) $ do
            of' $ \(Match _ _)      -> (TypeCheck.bindings          %~ (ref :))
                                     . (TypeCheck.untypedBinds      %~ (ref :))
            of' $ \(Var _)          ->  TypeCheck.unresolvedSymbols %~ (ref :)
            of' $ \(App _ _)        -> (TypeCheck.untypedApps       %~ (ref :))
                                     . (TypeCheck.uncalledApps      %~ (ref :))
            of' $ \(Acc _ _)        -> (TypeCheck.untypedAccs       %~ (ref :))
                                     . (TypeCheck.unresolvedSymbols %~ (ref :))
            of' $ \(Lit.String _)   ->  TypeCheck.untypedLits       %~ (ref :)
            of' $ \(Lit.Number _ _) ->  TypeCheck.untypedLits       %~ (ref :)
            of' $ \(Cons _ _)       ->  TypeCheck.untypedLits       %~ (ref :)
            of' $ \ANY              ->  id
    TypeCheck.modify_ mod

-- FIXME[MK]: This does not take pattern matching into account. To implement.
resolveLocalVars :: forall term node ls edge n e c m . PassCtx(m) => m ()
resolveLocalVars = do
    binds <- view TypeCheck.bindings <$> TypeCheck.get
    vars  <- fmap catMaybes $ forM binds $ \r -> do
        n :: node <- read r
        caseTest (uncover n) $ do
            of' $ \(Match l _) -> Just <$> follow source l
            of' $ \ANY         -> return Nothing
    TypeCheck.modify_ $ TypeCheck.unresolvedSymbols %~ (filter $ not . flip elem vars)

-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data ScanPass = ScanPass deriving (Show, Eq)

instance PassCtx(m) => TypeCheckerPass ScanPass m where
    hasJobs _ = not . null . view TypeCheck.freshRoots <$> TypeCheck.get

    runTCPass _ = do
        roots <- view TypeCheck.freshRoots <$> TypeCheck.get
        mapM_ (subtreeWalk investigate) roots
        resolveLocalVars
        TypeCheck.modify_ $ TypeCheck.freshRoots .~ []
        case roots of
            [] -> return Stuck
            _  -> return Progressed

