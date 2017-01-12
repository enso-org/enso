{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

module Luna.Pass.VarGatheringSpec (spec) where

import qualified Data.Set as Set

import           Control.Monad.Raise (tryAll)
import           Luna.Pass        (SubPass)
import qualified Luna.Pass        as Pass

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new)
import qualified Luna.Prelude as P
import qualified Luna.IR.Repr.Vis as Vis
import Data.TypeDesc
import Luna.TestUtils
import Luna.IR.Expr.Combinators
import Luna.IR.Expr.Layout.ENT hiding (Cons)
import Luna.IR.Function hiding (args)
import Luna.IR.Runner
import Luna.IR hiding (expr)
import Luna.Pass.Desugaring.RemoveGrouped
import System.Log


newtype UsedVars = UsedVars (Set.Set SomeExpr)

data VarGathering
type instance Abstract   VarGathering = VarGathering
type instance Pass.Inputs     Net   VarGathering = '[AnyExpr, AnyExprLink]
type instance Pass.Inputs     Layer VarGathering = '[AnyExpr // Model, AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Pass.Inputs     Attr  VarGathering = '[UsedVars]
type instance Pass.Inputs     Event VarGathering = '[]

type instance Pass.Outputs    Net   VarGathering = '[AnyExpr, AnyExprLink]
type instance Pass.Outputs    Layer VarGathering = '[AnyExpr // Model,  AnyExprLink // Model, AnyExpr // Type, AnyExpr // Succs]
type instance Pass.Outputs    Attr  VarGathering = '[UsedVars]
type instance Pass.Outputs    Event VarGathering = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink]

type instance Pass.Preserves        VarGathering = '[]


varsInside :: _ => SomeExpr -> SubPass VarGathering m [SomeExpr]
varsInside e = do
    f <- symbolFields e
    vars <- mapM (varsInside <=< source) f
    v <- match e $ \case
        Var{} -> return [e]
        _ -> return []
    return $ v ++ concat vars

varsNamesInside :: _ => SomeExpr -> SubPass VarGathering m [P.String]
varsNamesInside = varsInside >=> mapM varName

varName :: _ => SomeExpr -> SubPass VarGathering m P.String
varName e = match e $ \case
    Var n -> do
        n' <- source n
        match n' $ \case
            String s -> return s

gatherVars :: _ => [SomeExpr] -> SubPass VarGathering m _
gatherVars es = do
    varsNames <- mapM varsNamesInside es
    let uniqueVars = Set.toList $ Set.fromList $ concat varsNames
    vars <- mapM strVar uniqueVars
    forM_ vars $ \v ->
        forM_ es $ \e -> gatherVar (generalize v) e
    UsedVars s <- readAttr
    mapM_ deleteSubtree $ Set.toList $ Set.difference (Set.fromList $ map generalize vars) s

modifyAttr :: forall attr pass m. _ => (_ -> _) -> SubPass pass m ()
modifyAttr f = do
    st <- readAttr @attr
    writeAttr @attr $ f st

gatherVar :: _ => SomeExpr -> SomeExpr -> SubPass VarGathering m ()
gatherVar properVar expr = match expr $ \case
    Var n -> do
        sameVar <- sameNameVar properVar expr
        when sameVar $ do
            modifyAttr $ \(UsedVars s) -> UsedVars $ Set.insert properVar s
            replaceNode expr properVar
            deleteSubtree expr
    Acc n v -> do
        n' <- source n
        v' <- source v
        gatherVar properVar v'
    App f (Arg n v) -> do
        f' <- source f
        v' <- source v
        gatherVar properVar f'
        gatherVar properVar v'
    Lam (Arg n v) f -> do
        v' <- source v
        f' <- source f
        gatherVar v' f'
        sameVar <- sameNameVar properVar v'
        when (not sameVar) $ gatherVar properVar f'
    Grouped g -> source g >>= gatherVar properVar
    String{} -> return ()
    Integer{} -> return ()
    Rational{} -> return ()


sameNameVar :: _ => SomeExpr -> SomeExpr -> SubPass VarGathering m Bool
sameNameVar v1 v2 = match2 v1 v2 $ \x y -> case (x, y) of
    (Var n1, Var n2) -> do
        n1' <- source n1
        n2' <- source n2
        match2 n1' n2' $ \x y -> case (x, y) of
            (String s1, String s2) -> return $ s1 == s2
    _ -> return False


snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

desugarsTo :: _ => _ -> _ -> Expectation
desugarsTo test expected = do
    Right (res, coherence, orphans) <- tryAll $ withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        setAttr (getTypeDesc @UsedVars) $ UsedVars Set.empty
        x <- Pass.eval' test
        () <- Pass.eval' @VarGathering $ gatherVars $ map generalize x
        void $ Pass.eval' $ snapshotVis "desugar"
        orphans   <- Pass.eval' @RemoveGrouped $ checkUnreachableExprs $ map generalize x
        coherence <- Pass.eval' @RemoveGrouped checkCoherence
        expected' <- Pass.eval' expected
        result <- Pass.eval' $ fmap and $
            zipWithM (areExpressionsIsomorphic @(SubPass RemoveGrouped _))
                     (map unsafeRelayout expected')
                     (map unsafeRelayout x)
        return (result, coherence, orphans)
    res `shouldBe` True
    coherence `shouldSatisfy` null
    orphans `shouldSatisfy` null

lamXFoo :: _ => SubPass VarGathering _ _
lamXFoo = do
    x' <- strVar "x"
    ac <- rawAcc "foo" x'
    x <- strVar "x"
    l <- lam (arg x) ac
    return [l]

lamXFooExpected :: _ => SubPass VarGathering _ _
lamXFooExpected = do
    x <- strVar "x"
    ac <- rawAcc "foo" x
    l <- lam (arg x) ac
    return [l]

idLam :: _ => SubPass VarGathering _ _
idLam = do
    a <- strVar "a"
    a' <- strVar "a"
    l <- lam (arg a) a'
    return [l]

idLamExpected :: _ => SubPass VarGathering _ _
idLamExpected = do
    a <- strVar "a"
    l <- lam (arg a) a
    return [l]

lamFooAB :: _ => SubPass VarGathering _ _
lamFooAB = do
    a <- strVar "a"
    b <- strVar "b"
    foo <- strVar "foo"
    a1 <- app foo (arg a)
    a2 <- app a1 (arg b)
    a' <- strVar "a"
    b' <- strVar "b"
    l <- lam (arg b') a2
    l1 <- lam (arg a') l
    return [l1]

lamFooABExpected :: _ => SubPass VarGathering _ _
lamFooABExpected = do
    a <- strVar "a"
    b <- strVar "b"
    foo <- strVar "foo"
    a1 <- app foo (arg a)
    a2 <- app a1 (arg b)
    l <- lam (arg b) a2
    l1 <- lam (arg a) l
    return [l1]

foo1BarE :: _ => SubPass VarGathering _ _
foo1BarE = do
    foo <- strVar "foo"
    one <- integer (1::Int)
    a1 <- app foo (arg one)
    bar <- string "bar"
    a2 <- app a1 (arg bar)
    e <- rational 2.718
    a3 <- app a2 (arg e)
    return [a3]

groupedFooAAppA :: _ => SubPass VarGathering _ _
groupedFooAAppA = do
    foo <- strVar "foo"
    a <- strVar "quux"
    g <- app foo (arg a) >>= grouped
    a' <- strVar "quux"
    a1 <- app g (arg a')
    return [a1]

groupedFooAAppAExpected :: _ => SubPass VarGathering _ _
groupedFooAAppAExpected = do
    foo <- strVar "foo"
    a <- strVar "quux"
    g <- app foo (arg a) >>= grouped
    a1 <- app g (arg a)
    return [a1]


spec :: Spec
spec = describe "remove grouped" $ do
    it "\\x -> x.foo" $ lamXFoo `desugarsTo` lamXFooExpected
    it "\\a -> a" $ idLam `desugarsTo` idLamExpected
    it "\\a b -> foo a b" $ lamFooAB `desugarsTo` lamFooABExpected
    it "foo 1 \"bar\" 2.718" $ foo1BarE `desugarsTo` foo1BarE
    it "(foo a) a" $ groupedFooAAppA `desugarsTo` groupedFooAAppAExpected
