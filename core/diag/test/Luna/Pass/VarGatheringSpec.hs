{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

module Luna.Pass.VarGatheringSpec (spec) where

import qualified Data.Set as Set

import           Control.Monad.Raise (tryAll)
import           Luna.Pass        (SubPass)
import qualified Luna.Pass        as Pass

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, shouldSatisfy)
import Luna.Prelude hiding (String, s, new, cons)
import qualified Luna.Prelude as P
import qualified Luna.IR.Repr.Vis as Vis
import Data.TypeDesc
import Luna.TestUtils
import Luna.IR.Expr.Combinators
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


varsInside :: _ => SomeExpr -> SubPass VarGathering m [Expr Var]
varsInside e = do
    f <- symbolFields e
    vars <- mapM (varsInside <=< source) f
    v <- maybeToList <$> narrowAtom @Var e
    return $ v ++ concat vars

varsNamesInside :: _ => SomeExpr -> SubPass VarGathering m [P.String]
varsNamesInside = varsInside >=> mapM varName

varName :: _ => Expr Var -> SubPass VarGathering m P.String
varName e = fmap (view lit) . match' =<< source =<< view name <$> match' e

gatherVars :: _ => [SomeExpr] -> SubPass VarGathering m _
gatherVars es = do
    varsNames <- mapM varsNamesInside es
    let uniqueVars = Set.toList $ Set.fromList $ concat varsNames
    vars <- mapM strVar uniqueVars
    forM_ vars $ \v ->
        forM_ es $ \e -> gatherVar (generalize v) e
    UsedVars s <- readAttr
    let newVarsSet = Set.fromList $ map generalize vars
        unusedVars = Set.difference newVarsSet s
    mapM_ deleteSubtree $ Set.toList unusedVars
    return $ Set.toList $ Set.difference newVarsSet unusedVars


modifyAttr :: forall attr pass m. _ => (_ -> _) -> SubPass pass m ()
modifyAttr f = do
    st <- readAttr @attr
    writeAttr @attr $ f st

gatherVar :: _ => SomeExpr -> SomeExpr -> SubPass VarGathering m ()
gatherVar properVar expr = match expr $ \case
    Var{} -> do
        sameVar <- sameNameVar (unsafeRelayout properVar) (unsafeRelayout expr)
        when sameVar $ do
            modifyAttr $ \(UsedVars s) -> UsedVars $ Set.insert properVar s
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


sameNameVar :: _ => Expr Var -> Expr Var -> SubPass VarGathering m Bool
sameNameVar v1 v2 = do
    n1 <- varName v1
    n2 <- varName v2
    return $ n1 == n2


snapshotVis :: (MonadIR m, Vis.MonadVis m, MonadRef m) => P.String -> Pass.Pass TestPass m
snapshotVis = Vis.snapshot

desugarsTo :: _ => _ -> _ -> Expectation
desugarsTo test expected = do
    Right (res, coherence, orphans) <- tryAll $ withVis $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        setAttr (getTypeDesc @UsedVars) $ UsedVars Set.empty
        x <- Pass.eval' test
        -- void $ Pass.eval' $ snapshotVis "test"
        newReachables <- Pass.eval' @VarGathering $ gatherVars $ map generalize x
        void $ Pass.eval' $ snapshotVis "desugar"
        orphans   <- Pass.eval' @VarGathering $ checkUnreachableExprs $ newReachables ++ map generalize x
        coherence <- Pass.eval' @VarGathering checkCoherence
        expected' <- Pass.eval' expected
        -- void $ Pass.eval' $ snapshotVis "expected"
        result <- Pass.eval' $ fmap and $
            zipWithM (areExpressionsIsomorphic @(SubPass VarGathering _))
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

nEqFoo1BarE :: _ => SubPass VarGathering _ _
nEqFoo1BarE = do
    foo <- strVar "foo"
    one <- integer (1::Int)
    a1 <- app foo (arg one)
    bar <- string "bar"
    a2 <- app a1 (arg bar)
    e <- rational 2.718
    a3 <- app a2 (arg e)
    n <- strVar "n"
    u <- unify n a3
    return [u]

groupedFooAAppA :: _ => SubPass VarGathering _ _
groupedFooAAppA = do
    foo <- strVar "foo"
    a <- strVar "a"
    g <- app foo (arg a) >>= grouped
    a' <- strVar "a"
    a1 <- app g (arg a')
    return [a1]

groupedFooAAppAExpected :: _ => SubPass VarGathering _ _
groupedFooAAppAExpected = do
    foo <- strVar "foo"
    a <- strVar "a"
    g <- app foo (arg a) >>= grouped
    a1 <- app g (arg a)
    return [a1]

allAbove :: _ => SubPass VarGathering m [SomeExpr]
allAbove = do
    (t1 :: [SomeExpr]) <- map unsafeRelayout <$> lamXFoo
    (t2 :: [SomeExpr]) <- map unsafeRelayout <$> idLam
    (t3 :: [SomeExpr]) <- map unsafeRelayout <$> lamFooAB
    (t4 :: [SomeExpr]) <- map unsafeRelayout <$> nEqFoo1BarE
    (t5 :: [SomeExpr]) <- map unsafeRelayout <$> groupedFooAAppA
    return $ concat [t1, t2, t3, t4, t5]

allAboveExpected :: _ => SubPass VarGathering m [SomeExpr]
allAboveExpected = do
    (t1 :: [SomeExpr]) <- map unsafeRelayout <$> lamXFooExpected
    (t2 :: [SomeExpr]) <- map unsafeRelayout <$> idLamExpected
    (t3 :: [SomeExpr]) <- map unsafeRelayout <$> lamFooABExpected
    (t4 :: [SomeExpr]) <- map unsafeRelayout <$> nEqFoo1BarE
    (t5 :: [SomeExpr]) <- map unsafeRelayout <$> groupedFooAAppAExpected
    return $ concat [t1, t2, t3, t4, t5]

manyApps :: _ => SubPass VarGathering _ _
manyApps = do
    u1 <- unsafeRelayout <$> do
        foo <- strVar "foo"
        a <- strVar "a"
        n1 <- strVar "n1"
        unify n1 =<< app foo (arg a)
    u2 <- unsafeRelayout <$> do
        bar <- strVar "bar"
        a <- strVar "a"
        b <- strVar "b"
        ap1 <- app bar (arg a)
        n2 <- strVar "n2"
        unify n2 =<< app ap1 (arg b)
    u3 <- unsafeRelayout <$> do
        baz <- strVar "baz"
        a <- strVar "a"
        b <- strVar "b"
        c <- strVar "c"
        ap1 <- app baz (arg a)
        ap2 <- app ap1 (arg b)
        ap3 <- app ap2 (arg c)
        n3 <- strVar "n3"
        unify n3 ap3
    return [u1, u2, u3]

manyAppsExpected :: _ => SubPass VarGathering _ _
manyAppsExpected = do
    a <- strVar "a"
    b <- strVar "b"
    u1 <- unsafeRelayout <$> do
        foo <- strVar "foo"
        n1 <- strVar "n1"
        unify n1 =<< app foo (arg a)
    u2 <- unsafeRelayout <$> do
        bar <- strVar "bar"
        ap1 <- app bar (arg a)
        n2 <- strVar "n2"
        unify n2 =<< app ap1 (arg b)
    u3 <- unsafeRelayout <$> do
        baz <- strVar "baz"
        c <- strVar "c"
        ap1 <- app baz (arg a)
        ap2 <- app ap1 (arg b)
        ap3 <- app ap2 (arg c)
        n3 <- strVar "n3"
        unify n3 ap3
    return [u1, u2, u3]

lamPattern :: _ => SubPass VarGathering _ _
lamPattern = do
    topA <- strVar "a"
    list <- string "Tuple3"
    a <- strVar "a"
    b <- strVar "b"
    c <- strVar "c"
    pat <- cons list $ map arg [a,b,c]
    b' <- strVar "b"
    l <- lam (arg pat) b'
    return [unsafeRelayout topA, unsafeRelayout l]

lamPatternExpected :: _ => SubPass VarGathering _ _
lamPatternExpected = do
    topA <- strVar "a"
    list <- string "Tuple3"
    a <- strVar "a"
    b <- strVar "b"
    c <- strVar "c"
    pat <- cons list $ map arg [a,b,c]
    l <- lam (arg pat) b
    return [unsafeRelayout topA, unsafeRelayout l]

simpleLamPattern :: _ => SubPass VarGathering _ _
simpleLamPattern = do
    box <- string "Box"
    a <- strVar "a"
    pat <- cons box [arg a]
    a' <- strVar "a"
    l <- lam (arg pat) a'
    return [l]

simpleLamPatternExpected :: _ => SubPass VarGathering _ _
simpleLamPatternExpected = do
    box <- string "Box"
    a <- strVar "a"
    pat <- cons box [arg a]
    l <- lam (arg pat) a
    return [l]

aWithSimpleLamPattern :: _ => SubPass VarGathering _ _
aWithSimpleLamPattern = do
    topA <- strVar "a"
    box <- string "Box"
    a <- strVar "a"
    pat <- cons box [arg a]
    a' <- strVar "a"
    l <- lam (arg pat) a'
    return [unsafeRelayout topA, unsafeRelayout l]

aWithSimpleLamPatternExpected :: _ => SubPass VarGathering _ _
aWithSimpleLamPatternExpected = do
    topA <- strVar "a"
    box <- string "Box"
    a <- strVar "a"
    pat <- cons box [arg a]
    l <- lam (arg pat) a
    return [unsafeRelayout topA, unsafeRelayout l]


spec :: Spec
spec = describe "gather vars" $ do
    it "\\x -> x.foo" $ lamXFoo `desugarsTo` lamXFooExpected
    it "\\a -> a" $ idLam `desugarsTo` idLamExpected
    it "\\a b -> foo a b" $ lamFooAB `desugarsTo` lamFooABExpected
    it "n = foo 1 \"bar\" 2.718" $ nEqFoo1BarE `desugarsTo` nEqFoo1BarE
    it "(foo a) a" $ groupedFooAAppA `desugarsTo` groupedFooAAppAExpected
    it "all of the above" $ allAbove `desugarsTo` allAboveExpected
    it "n1 = foo a; n2 = bar a b; n3 = baz a b c" $ manyApps `desugarsTo` manyAppsExpected
    it "\\(Box a) -> a" $ simpleLamPattern `desugarsTo` simpleLamPatternExpected
    it "a; \\(Box a) -> a" $ aWithSimpleLamPattern `desugarsTo` aWithSimpleLamPatternExpected
    it "a; \\(Tuple3 a b c) -> b" $ lamPattern `desugarsTo` lamPatternExpected
