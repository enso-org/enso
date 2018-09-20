{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Pass.Typing.UniSolver where

import Prologue

import qualified Control.Monad.State.Layered      as State
import qualified Control.Monad.Trans.Except       as Except
import qualified Data.Graph.Data.Component.Vector as ComponentVector
import qualified Data.Graph.Data.Graph.Class      as Graph
import qualified Data.Graph.Data.Layer.Layout     as Layout
import qualified Data.Set                         as Set
import qualified Luna.IR                          as IR
import qualified Luna.IR.Aliases                  as Uni
import qualified Luna.IR.Layer                    as Layer
import qualified Luna.Pass                        as Pass
import qualified Luna.Pass.Attr                   as Attr
import qualified Luna.Pass.Data.Error             as Error
import qualified Luna.Pass.Data.Layer.Requester   as Requester
import qualified Luna.Pass.Data.Stage             as TC
import qualified Luna.Pass.Typing.Base            as TC
import qualified Luna.Pass.Typing.Data.Progress   as Progress
import qualified Luna.Pass.Typing.Data.UniQueue   as UniQueue
import qualified Luna.Syntax.Prettyprint          as Pretty

symmetrical :: Applicative f => (a -> a -> f b) -> a -> a -> f b
symmetrical f a b = f a b *> f b a

newtype ResolutionT r m u
    = ResolutionT (Except.ExceptT r m u)
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)
makeLenses ''ResolutionT

type instance Graph.Discover (ResolutionT r m) = Graph.Discover m


class Monad m => MonadResolution r m | m -> r where
    resolve :: r -> m ()

instance Monad m => MonadResolution r (ResolutionT r m) where
    resolve = wrap . Except.throwE


resolve_ :: (MonadResolution a m, Mempty a) => m ()
resolve_ = resolve mempty

runResolutionT :: Monad m => ResolutionT r m u -> m (Either r u)
runResolutionT = Except.runExceptT . unwrap

data UniSolver

type instance Pass.Spec UniSolver t = UniSolverSpec t
type family UniSolverSpec t where
    UniSolverSpec (Pass.In  Pass.Attrs) = '[UniQueue.UniQueue]
    UniSolverSpec (Pass.Out Pass.Attrs) = '[ UniQueue.UniQueue
                                           , Progress.Progress
                                           ]
    UniSolverSpec t = TC.BasePassSpec t

instance Pass.Definition TC.Stage UniSolver where
    definition = do
        UniQueue.UniQueue queue <- Attr.get
        new <- concat <$> traverse deepSolve queue
        Attr.put $ UniQueue.UniQueue new
        Attr.put $ Progress.Progress $ queue /= new

type SolverRule m = IR.Term IR.Unify -> IR.SomeTerm -> IR.SomeTerm -> ResolutionT [IR.Term IR.Unify] (TC.Pass UniSolver) ()

deleteUni :: SolverRule m
deleteUni uni a b = IR.deleteSubtreeWithWhitelist (Set.fromList [a, b]) uni

resolveError :: SolverRule m
resolveError uni a b = do
    aRep <- Pretty.printType a
    bRep <- Pretty.printType b
    requester <- Requester.getRequester uni
    arising   <- Requester.getArising   uni
    for_ requester $ Error.setError $ Just
        $ Error.unificationError aRep bRep
            & Error.arisingFrom .~ arising
    IR.deleteSubtree uni
    resolve_

reflexivity :: SolverRule m
reflexivity uni a b = when (a == b) $ do
    IR.deleteSubtreeWithWhitelist (Set.fromList [a, b]) uni
    resolve_

variable :: SolverRule m
variable uni a b = do
    Layer.read @IR.Model a >>= \case
        Uni.Var {} -> do
            let go = do
                  traverse (IR.replace b) [a, Layout.relayout uni]
                  resolve_
            Layer.read @IR.Model b >>= \case
                Uni.Var          {} -> go
                Uni.ResolvedCons {} -> go
                Uni.Lam          {} -> go
                _ -> return ()
        _ -> return ()

lambda :: SolverRule m
lambda uni a b = do
    modA <- Layer.read @IR.Model a
    modB <- Layer.read @IR.Model b
    case (modA, modB) of
        (Uni.Lam arg1 out1, Uni.Lam arg2 out2) -> do
            uniA <- join $ IR.unify <$> IR.source arg1 <*> IR.source arg2
            uniO <- join $ IR.unify <$> IR.source out1 <*> IR.source out2
            IR.deleteSubtree uni
            resolve [ Layout.unsafeRelayout uniA
                    , Layout.unsafeRelayout uniO
                    ]
        (Uni.Lam {}, _) -> resolveError uni a b
        (_, Uni.Lam {}) -> resolveError uni a b
        _ -> return ()

cons :: SolverRule m
cons uni a b = do
    modA <- Layer.read @IR.Model a
    modB <- Layer.read @IR.Model b
    case (modA, modB) of
        (Uni.ResolvedCons mod1 n1 _ fs1, Uni.ResolvedCons mod2 n2 _ fs2) -> do
            if n1 == n2 && mod1 == mod2 then do
                ins1 <- traverse IR.source =<< ComponentVector.toList fs1
                ins2 <- traverse IR.source =<< ComponentVector.toList fs2
                unis <- zipWithM IR.unify ins1 ins2
                IR.deleteSubtree uni
                resolve $ Layout.unsafeRelayout <$> unis
            else resolveError uni a b
        _ -> return ()

allRules :: SolverRule m
allRules uni l r = do
    reflexivity uni l r
    symmetrical (variable uni) l r
    lambda uni l r
    cons   uni l r

solveUnification :: IR.Term IR.Unify -> TC.Pass UniSolver (Either [IR.Term IR.Unify] ())
solveUnification uni = do
    req     <- Requester.getRequester uni
    arising <- Requester.getArising   uni
    IR.Unify l' r' <- IR.modelView uni
    l <- IR.source l'
    r <- IR.source r'
    solution <- runResolutionT $ allRules uni l r
    case solution of
        Left new -> do
            traverse_ (Requester.setRequester req)     new
            traverse_ (Requester.setArising   arising) new
        Right _ -> return ()
    return solution

deepSolve :: IR.Term IR.Unify -> TC.Pass UniSolver [IR.Term IR.Unify]
deepSolve uni = do
    res <- solveUnification uni
    case res of
        Left new -> concat <$> traverse deepSolve new
        Right _  -> return [uni]

