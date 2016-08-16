{-# LANGUAGE CPP                       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Compilation.Pass.Inference.Scan where
import Prelude.Luna
import Data.Construction
import Data.Prop
import Data.Record.Match
import Data.Graph
import Data.Graph.Builder
import Data.Maybe                                        (catMaybes)
import qualified Data.Graph.Backend.NEC                  as NEC

import Old.Luna.Syntax.Term.Class                            hiding (source, target)
import Luna.Syntax.Model.Layer
import Luna.Runtime.Dynamics                         (Static)
import Luna.Syntax.Model.Network.Class                 ()
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Term

import Luna.Compilation.Pass.Utils.SubtreeWalk         (subtreeWalk)
import Luna.Syntax.Model.Network.Builder               (TCDataPayload, depth, redirect, replaceNodeNonDestructive, replaceNode, HasSuccs)

import           Luna.Compilation.Stage.TypeCheck                (ProgressStatus (..), TypeCheckerPass, hasJobs, runTCPass)
import           Luna.Compilation.Stage.TypeCheck.Class          (MonadTypeCheck)
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheck
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit
import           Data.Layer_OLD.Cover_OLD

#define PassCtx(m) ( term ~ Draft Static                           \
                   , Covered node                                  \
                   , node ~ (ls :<: term)                          \
                   , edge ~ Link node                              \
                   , BiCastable n node                             \
                   , BiCastable e edge                             \
                   , Covered node                                  \
                   , Destructor m (Ref Edge edge)                  \
                   , Destructor m (Ref Node node)                  \
                   , Connectible (Ref Node node) (Ref Node node) m \
                   , MonadBuilder (Hetero (NEC.Graph n e c)) m     \
                   , NodeInferable  (m) node                       \
                   , HasSuccs node                                 \
                   , TermNode Lam   m (ls :<: term)                \
                   , TermNode Blank m (ls :<: term)                \
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
            of' $ \(Acc _ _)        ->  TypeCheck.untypedAccs       %~ (ref :)
            of' $ \(Lam _ _)        ->  TypeCheck.untypedLambdas    %~ (ref :)
            of' $ \(Lit.String _)   ->  TypeCheck.untypedLits       %~ (ref :)
            of' $ \(Lit.Number _ _) ->  TypeCheck.untypedLits       %~ (ref :)
            of' $ \(Cons _ _)       ->  TypeCheck.untypedLits       %~ (ref :)
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

isBlank :: PassCtx(m) => Ref Node node -> m Bool
isBlank r = do
    n <- read r
    caseTest (uncover n) $ do
        of' $ \Blank -> return True
        of' $ \ANY   -> return False


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

resolveLambdaBinders :: PassCtx(m) => m ()
resolveLambdaBinders = do
    lambdas   <- view TypeCheck.untypedLambdas <$> TypeCheck.get
    boundVars <- fmap concat $ forM lambdas $ \ref -> do
        n <- read ref
        caseTest (uncover n) $ do
            of' $ \(Lam as _) -> mapM (follow source . unlayer) as
            of' $ \ANY -> impossible
    TypeCheck.modify_ $ TypeCheck.unresolvedSymbols %~ (filter $ not . flip elem boundVars)

replaceVar :: PassCtx(m) => Ref Node node -> String -> Ref Node node -> m ()
replaceVar root name replacement = do
    nroot <- read root
    forM_ (nroot # Inputs) $ \inp -> do
        nref <- follow source inp
        replaceVar nref name replacement
    caseTest (uncover nroot) $ do
        of' $ \(Var (Lit.String n)) -> if n == name && root /= replacement
            then do
                replaceNode root replacement
                TypeCheck.modify_ $ (TypeCheck.unresolvedSymbols %~ (filter (/= root)))
                                  . (TypeCheck.allNodes %~ (filter (/= root)))
            else return ()
        of' $ \ANY -> return ()

unifyLambdaArguments :: PassCtx(m) => m ()
unifyLambdaArguments = do
    lambdas <- view TypeCheck.untypedLambdas <$> TypeCheck.get
    forM_ lambdas $ \ref -> do
        n <- read ref
        caseTest (uncover n) $ do
            of' $ \(Lam as o) -> do
                args <- mapM (follow source . unlayer) as
                toReplace <- fmap catMaybes $ forM args $ \rv -> do
                    nv <- read rv
                    return $ caseTest (uncover nv) $ do
                        of' $ \(Var (Lit.String name)) -> Just (name, rv)
                        of' $ \ANY -> Nothing
                out <- follow source o
                mapM (uncurry $ replaceVar out) toReplace
            of' $ \ANY -> impossible


desugarLambdas :: PassCtx(m) => m ()
desugarLambdas = do
    accs <- view TypeCheck.untypedAccs <$> TypeCheck.get
    forM_ accs $ \ref -> do
        acc <- read ref
        caseTest (uncover acc) $ do
            of' $ \(Acc _ t) -> do
                tgt <- follow source t
                isB <- isBlank tgt
                when isB $ do
                    tempBlank <- blank
                    replaceNodeNonDestructive ref tempBlank
                    lambda <- lam [arg tgt] ref
                    replaceNode tempBlank lambda
                    TypeCheck.modify_ $ TypeCheck.untypedLambdas %~ (lambda :)
            of' $ \ANY -> impossible

    apps <- view TypeCheck.untypedApps <$> TypeCheck.get
    forM_ apps $ \ref -> do
        node <- read ref
        caseTest (uncover node) $ do
            of' $ \(App _ as) -> do
                args   <- mapM (follow source . unlayer) as
                blanks <- filterM isBlank args
                when (not . null $ blanks) $ do
                    tempBlank <- blank
                    replaceNodeNonDestructive ref tempBlank
                    lambda <- lam (arg <$> blanks) ref
                    replaceNode tempBlank lambda
                    TypeCheck.modify_ $ TypeCheck.untypedLambdas %~ (lambda :)
            of' $ \ANY -> impossible


-----------------------------
-- === TypeCheckerPass === --
-----------------------------

data ScanPass = ScanPass deriving (Show, Eq)

instance (Monad m {-ghc8-}, PassCtx(m)) => TypeCheckerPass ScanPass m where
    hasJobs _ = not . null . view TypeCheck.freshRoots <$> TypeCheck.get

    runTCPass _ = do
        roots <- view TypeCheck.freshRoots <$> TypeCheck.get
        mapM_ (subtreeWalk investigate) roots
        resolveLocalVars
        resolveLambdaBinders
        unifyLambdaArguments
        desugarLambdas
        mapM_ assignDepths roots
        TypeCheck.modify_ $ TypeCheck.freshRoots .~ []
        case roots of
            [] -> return Stuck
            _  -> return Progressed
