module Luna.Pass.UnificationSolverSpec where

import Luna.Prelude as P hiding (cons)
import Test.Hspec   (Spec, describe, it, pending, shouldBe, shouldReturn, Expectation)
import Data.TypeDesc
import Luna.TestUtils
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events)
import qualified Luna.Pass        as Pass
import Luna.IR.Runner
import Luna.IR
import Luna.IR.Function
import Luna.IR.Expr.Combinators
import System.Log
import Control.Monad.Raise
import Control.Monad.Trans.Either
import Data.Maybe (isJust)
import Luna.IR.Expr.Term.Named (Term (Sym_Unify))

data UnificationSolver
type instance Abstract      UnificationSolver = UnificationSolver
type instance Inputs  Net   UnificationSolver = '[AnyExpr, AnyExprLink]
type instance Outputs Net   UnificationSolver = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer UnificationSolver = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer UnificationSolver = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Succs, AnyExpr // Type, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  UnificationSolver = '[WorkingUni]
type instance Outputs Attr  UnificationSolver = '[]
type instance Inputs  Event UnificationSolver = '[]
type instance Outputs Event UnificationSolver = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink]
type instance Preserves     UnificationSolver = '[]

data WorkingUni = WorkingUni (Expr $ E Unify)

type SubPassReq k t m = (Readers k (Inputs k t) m, Writers k (Outputs k t) m)

type MonadSubPass t m = (SubPassReq Net t m, SubPassReq Layer t m, SubPassReq Attr t m, Emitters (Outputs Event t) m)

class Monad m => MonadResolution r m | m -> r where
    resolve :: r -> m ()

resolve_ :: MonadResolution [t] m => m ()
resolve_ = resolve []

newtype ResolutionT r m u = ResolutionT (EitherT r m u) deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadTrans)

makeWrapped ''ResolutionT

runResolutionT :: Monad m => ResolutionT r m u -> m (Either r u)
runResolutionT = runEitherT . unwrap'

instance Monad m => MonadResolution r (ResolutionT r m) where
    resolve = wrap' . left

symmetrical :: Applicative f => (a -> a -> f b) -> a -> a -> f b
symmetrical f a b = f a b *> f b a

reflexivity :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [SomeExpr] m) => SomeExpr -> SomeExpr -> SomeExpr -> m ()
reflexivity uni a b = when (a == b) $ do
    deleteWithoutInputs uni
    resolve_

variable :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [SomeExpr] m) => SomeExpr -> SomeExpr -> SomeExpr -> m ()
variable uni a b = do
    isVar <- isJust <$> narrowAtom @Var a
    when isVar $ do
        replaceNode   a b
        deleteWithoutInputs uni
        deleteSubtree a
        resolve_

lambda :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [SomeExpr] m) => SomeExpr -> SomeExpr -> SomeExpr -> m ()
lambda uni a b = match2 a b $ \x y -> case (x, y) of
    (Lam (Arg _ a1) o1, Lam (Arg _ a2) o2) -> do
        arg1 <- source a1
        arg2 <- source a2
        out1 <- source o1
        out2 <- source o2
        uniA <- unify arg1 arg2
        uniO <- unify out1 out2
        deleteWithoutInputs uni
        resolve $ generalize <$> [uniA, uniO]
    _ -> return ()

solveUnification :: (MonadRef m, MonadPassManager m, MonadSubPass UnificationSolver m, MonadResolution [SomeExpr] m) => Expr (E Unify) -> m ()
solveUnification uni = do
    let uni' = generalize uni
    Sym_Unify l' r' <- match' uni
    l <- source $ (unsafeGeneralize l' :: SomeExprLink)
    r <- source $ (unsafeGeneralize r' :: SomeExprLink)
    reflexivity uni' l r
    symmetrical (variable uni') l r
    lambda uni' l r


spec = do
    describe "nothing" $
        it "nothings" $ 1 `shouldBe` 1

