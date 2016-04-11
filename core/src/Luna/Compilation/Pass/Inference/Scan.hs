{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Scan where
import Prelude.Luna
import Data.Construction
import Data.Prop
import Data.Record.Match
import Data.Graph
import Data.Graph.Builder
import qualified Data.Graph.Backend.NEC                  as NEC

import Luna.Syntax.Term.Class_OLD                            hiding (source, target)
import Luna.Syntax.Model.Layer
import Luna.Runtime.Dynamics                         (Static)
import Luna.Syntax.Model.Network.Class                 ()
import Luna.Syntax.Model.Network.Builder.Node.Inferred
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Term

import Luna.Compilation.Pass.Utils.SubtreeWalk         (subtreeWalk)
import Luna.Syntax.Model.Network.Builder.Layer         (TCDataPayload, depth, redirect)

import           Luna.Compilation.Stage.TypeCheck                (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class          (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheck
import qualified Luna.Syntax.Term.Lit                        as Lit
import           Data.Layer_OLD.Cover_OLD

#define PassCtx(m) ( term ~ Draft Static                           \
                   , Covered node                                  \
                   , node ~ (ls :<: term)                          \
                   , edge ~ Link node                              \
                   , BiCastable n node                             \
                   , BiCastable e edge                             \
                   , Destructor m (Ref Edge edge)                  \
                   , Connectible (Ref Node node) (Ref Node node) m \
                   , MonadBuilder (Hetero (NEC.Graph n e c)) m     \
                   , NodeInferable  (m) node                       \
                   , Getter Inputs node                            \
                   , Prop Inputs node ~ [Ref Edge edge]            \
                   , HasProp TCData node                           \
                   , Prop TCData node ~ TCDataPayload node         \
                   , MonadTypeCheck (ls :<: term) m                \
                   , ReferencedM Node (Hetero (NEC.Graph n e c)) m (ls :<: term) \
                   , ReferencedM Edge (Hetero (NEC.Graph n e c)) m edge \
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
            of' $ \(Curry _ _)      ->  TypeCheck.uncalledCurries   %~ (ref :)
            of' $ \ANY              ->  id
    TypeCheck.modify_ $ mod . (TypeCheck.allNodes %~ (ref :))

assignDepths :: PassCtx(m) => Ref Node node -> m Int
assignDepths ref = do
    currentDepth <- follow (prop TCData . depth) ref
    case currentDepth of
        Just d  -> return d
        Nothing -> do
            withRef ref $ prop TCData . depth ?~ -1 -- this serves as an unobtrusive "visited" flag. Needed because of the terrible behavior of our Star node.
            n <- read ref
            redir <- follow (prop TCData . redirect) ref
            let edges = maybeToList redir ++ (n # Inputs)
            predDeps <- mapM (follow source >=> assignDepths) edges
            let newDep = 1 + maximum (-1 : predDeps)
            withRef ref $ prop TCData . depth ?~ newDep
            return newDep


-- FIXME[MK]: This does not take pattern matching into account. To implement.
resolveLocalVars :: forall term node ls edge n e c m . PassCtx(m) => m ()
resolveLocalVars = do
    binds <- view TypeCheck.bindings <$> TypeCheck.get
    vars  <- fmap catMaybes $ forM binds $ \r -> do
        n :: node <- read r
        caseTest (uncover n) $ do
            of' $ \(Match l r) -> do
                ln <- follow source l
                rn <- follow source r
                reconnect (prop TCData . redirect) ln $ Just rn
                return $ Just ln
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
        mapM_ assignDepths roots
        TypeCheck.modify_ $ TypeCheck.freshRoots .~ []
        case roots of
            [] -> return Stuck
            _  -> return Progressed
