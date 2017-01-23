module Luna.Pass.Desugaring.AliasAnalysis where

import qualified Data.Set as Set

import           Luna.Pass        (SubPass)
import qualified Luna.Pass        as Pass

import Luna.Prelude hiding (String, s, new, cons)
import qualified Luna.Prelude as P
import qualified Luna.IR.Repr.Vis as Vis
import Data.TypeDesc
import Luna.IR.Expr.Combinators
import Luna.IR.Function hiding (args)
import Luna.IR hiding (expr)
import System.Log



newtype UsedVars = UsedVars (Set.Set (Expr Var))

data AliasAnalysis
type instance Abstract              AliasAnalysis = AliasAnalysis
type instance Pass.Inputs     Net   AliasAnalysis = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer AliasAnalysis = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Pass.Inputs     Attr  AliasAnalysis = '[UsedVars]
type instance Pass.Inputs     Event AliasAnalysis = '[]

type instance Pass.Outputs    Net   AliasAnalysis = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer AliasAnalysis = '[AnyExpr // Model,  AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Pass.Outputs    Attr  AliasAnalysis = '[UsedVars]
type instance Pass.Outputs    Event AliasAnalysis = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink]

type instance Pass.Preserves        AliasAnalysis = '[]

gatherVars :: (MonadRef m, MonadPassManager m) => [SomeExpr] -> SubPass AliasAnalysis m [Expr Var]
gatherVars es = do
    varsNames <- mapM varsNamesInside es
    let uniqueVars = Set.toList $ Set.fromList $ concat varsNames
    vars <- mapM strVar uniqueVars
    forM_ vars $ \v ->
        forM_ es $ \e -> gatherVar (generalize v) e
    UsedVars s <- readAttr
    let newVarsSet :: Set.Set (Expr Var)
        newVarsSet = Set.fromList $ map unsafeGeneralize vars
        unusedVars = Set.difference newVarsSet s
    mapM_ deleteSubtree $ Set.toList unusedVars
    return $ Set.toList $ Set.difference newVarsSet unusedVars

varsInside :: MonadRef m => SomeExpr -> SubPass AliasAnalysis m [Expr Var]
varsInside e = do
    f <- symbolFields e
    vars <- mapM (varsInside <=< source) f
    v <- maybeToList <$> narrowAtom @Var e
    return $ v ++ concat vars

varsNamesInside :: MonadRef m => SomeExpr -> SubPass AliasAnalysis m [P.String]
varsNamesInside = varsInside >=> mapM varName

varName :: MonadRef m => Expr Var -> SubPass AliasAnalysis m P.String
varName e = fmap (view lit) . match' =<< source =<< view name <$> match' e

modifyAttr :: forall m. MonadRef m => (RefData' Attr UsedVars (SubPass AliasAnalysis m) -> RefData' Attr UsedVars (SubPass AliasAnalysis m)) -> SubPass AliasAnalysis m ()
modifyAttr f = do
    st <- readAttr
    writeAttr $ f st

gatherVar :: MonadPassManager m => SomeExpr -> SomeExpr -> SubPass AliasAnalysis m ()
gatherVar properVar expr = match expr $ \case
    Var{} -> do
        sameVar <- sameNameVar (unsafeRelayout properVar) (unsafeRelayout expr)
        when sameVar $ do
            modifyAttr $ \(UsedVars s) -> UsedVars $ Set.insert (unsafeRelayout properVar) s
            replaceNode expr properVar
            deleteSubtree expr
    Acc _ v -> do
        v' <- source v
        gatherVar properVar v'
    App f (Arg _ v) -> do
        f' <- source f
        v' <- source v
        gatherVar properVar f'
        gatherVar properVar v'
    Lam (Arg _ v) f -> do
        v' <- source v
        f' <- source f
        match v' $ \case
            Var{} -> do
                gatherVar v' f'
                sameVar <- sameNameVar (unsafeRelayout properVar) (unsafeRelayout v')
                when (not sameVar) $ gatherVar properVar f'
            Cons _n args -> do
                atLeastOneIsEqual <- forM args $ \(Arg _ n) -> do
                    n' <- source n
                    gatherVar n' f'
                    sameVar <- sameNameVar (unsafeRelayout properVar) (unsafeRelayout n')
                    return sameVar
                when (and $ map not atLeastOneIsEqual) $ gatherVar properVar f'
    Grouped g -> source g >>= gatherVar properVar
    Unify l r -> do
        source l >>= gatherVar properVar
        source r >>= gatherVar properVar
    Cons n _args -> source n >>= gatherVar properVar
    String{} -> return ()
    Integer{} -> return ()
    Rational{} -> return ()
    Blank{} -> return ()
    Star{} -> return ()
    Missing{} -> return ()


sameNameVar :: MonadRef m => Expr Var -> Expr Var -> SubPass AliasAnalysis m Bool
sameNameVar v1 v2 = do
    n1 <- varName v1
    n2 <- varName v2
    return $ n1 == n2
