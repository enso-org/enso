{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Luna.Pass.BlankDesugarSpec (spec) where

import           Luna.Pass        (Pass, SubPass, Inputs, Outputs, Preserves, Events, setAttr3)
import qualified Luna.Pass        as Pass

import Test.Hspec   (Spec, Expectation, describe, it, shouldBe, pending)
import Luna.Prelude hiding (String)
import qualified Luna.Prelude as P
import Data.TypeVal
import Luna.TestUtils
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Luna.IR.Function.Definition as Function
import Luna.IR.Function
import Luna.IR.Module.Definition   as Module
import Luna.IR.Runner
import Luna.IR
import Luna.IR.Expr.Combinators
import Luna.IR.Name                (Name)
import System.Log
import Control.Monad (foldM, replicateM)
import Data.Maybe (isJust)
import Debug.Trace (traceM)


data UniqueNameGen
type instance PassAttr UniqueNameGen BlankDesugaring = (P.String, Int)

data UsedVars
type instance PassAttr UsedVars BlankDesugaring = [P.String]

genName :: (IRMonad m, MonadPassManager m) => SubPass BlankDesugaring m P.String
genName = do
    (base, number) <- readAttr @UniqueNameGen
    setAttr3 @UniqueNameGen $ (base, number + 1)
    return $ '#' : base ++ show number

obscureName :: P.String
obscureName = "#obscureName0"

data BlankDesugaring
type instance Abstract  BlankDesugaring = BlankDesugaring
type instance Inputs    BlankDesugaring = '[ExprNet, ExprLinkNet] <> ExprLayers '[Model] <> ExprLinkLayers '[Model] <> '[Attr UniqueNameGen, Attr UsedVars]
type instance Outputs   BlankDesugaring = '[Attr UniqueNameGen, Attr UsedVars]
type instance Events    BlankDesugaring = '[NEW // EXPR, NEW // LINK' EXPR]
type instance Preserves BlankDesugaring = '[]

blankDotFoo :: _ => SubPass TestPass m _
blankDotFoo = do
    b <- blank
    expr <- rawAcc "foo" b
    return expr

blankDotFooExpected :: _ => SubPass TestPass m _
blankDotFooExpected = do
    v <- strVar obscureName
    a <- rawAcc "foo" v
    l <- lam (arg v) a
    return l

isBlank :: _ => _
isBlank e = isJust <$> narrowAtom @Blank e

blankInside :: _ => AnyExpr -> SubPass BlankDesugaring m Bool
blankInside e = match e $ \case
    App a (Arg _ b) -> do
        aTarget <- source a
        aBlank <- isBlank aTarget
        bTarget <- source b
        if aBlank then return True else blankInside bTarget
    Blank -> return True
    _ -> return False

countBlankInside :: _ => AnyExpr -> SubPass BlankDesugaring m Int
countBlankInside e = countBlankInside' 0 e

countBlankInside' :: _ => Int -> AnyExpr -> SubPass BlankDesugaring m Int
countBlankInside' i e = match e $ \case
    App a (Arg _ b) -> do
        aTarget <- source a
        aBlank <- countBlankInside' 0 aTarget
        bTarget <- source b
        blanksInsideB <- countBlankInside' 0 bTarget
        return $ aBlank + blanksInsideB
    Acc n t -> do
        tTarget <- source t
        countBlankInside' i tTarget
    Blank -> return $ i+1
    _ -> return i

replaceBlanks :: forall m. (IRMonad m, MonadPassManager m)
              => AnyExpr -> SubPass BlankDesugaring m AnyExpr
replaceBlanks e = match e $ \case
    App f (Arg _ a) -> do
        f' <- source f >>= replaceBlanks
        a' <- source a >>= replaceBlanks
        unsafeRelayout <$> app f' (arg a')
    Blank -> do
        n <- genName
        st <- readAttr @UsedVars
        setAttr3 @UsedVars (st ++ [n])
        unsafeRelayout <$> strVar n
    Acc n v -> do
        n' <- source n >>= replaceBlanks
        v' <- source v >>= replaceBlanks
        unsafeRelayout <$> acc n' v'
    Grouped g -> do
        g' <- source g
        st <- readAttr @UsedVars
        setAttr3 @UsedVars []
        desu <- desugar g'
        setAttr3 @UsedVars st
        unsafeRelayout <$> grouped desu
    _ -> return e

printExpr :: _ => AnyExpr -> SubPass BlankDesugaring m P.String
printExpr e = match e $ \case
    App f (Arg _ a) -> do
        f' <- source f >>= printExpr
        a' <- source a >>= printExpr
        return $ "(App " ++ f' ++ " " ++ a' ++ ")"
    Acc n v -> do
        n' <- source n >>= printExpr
        v' <- source v >>= printExpr
        return $ "(Acc " ++ n' ++ " " ++ v' ++ ")"
    Var n -> do
        n' <- source n >>= printExpr
        return $ "(Var " ++ n' ++ ")"
    Lam (Arg _ a) l -> do
        a' <- source a >>= printExpr
        l' <- source l >>= printExpr
        return $ "(Lam \\" ++ a' ++ " -> " ++ l' ++ ")"
    String n -> return n
    Integer i -> return $ show i
    Grouped g -> do
        g' <- source g >>= printExpr
        return $ "(Grouped " ++ g' ++ ")"
    Blank -> return "_"
    _ -> return "UNKNOWN"

lams :: _ => [AnyExpr] -> AnyExpr -> m AnyExpr
lams args output = unsafeRelayout <$> foldM f (unsafeRelayout output) (unsafeRelayout <$> reverse args)
    where
        f arg' lam' = lamAny (arg lam') arg'

lamAny :: _ => Arg AnyExpr -> AnyExpr -> m AnyExpr
lamAny a b = fmap generalize $ lam a b

desugar :: forall m. (IRMonad m, MonadPassManager m, _)
        => AnyExpr -> SubPass BlankDesugaring m (AnyExpr)
desugar e = do
    traceM "DESUGARING:"
    traceM =<< printExpr e
    traceM "-----------"
    blankArgs <- countBlankInside e
    -- names <- do
    --     st <- readAttr @UniqueNameGen
    --     ns <- replicateM blankArgs genName
    --     setAttr3 @UniqueNameGen st
    --     return ns
    e' <- replaceBlanks e
    names <- readAttr @UsedVars
    vars <- mapM strVar names
    lams (map unsafeRelayout vars) e'


desugarsTo :: _ => _ -> _ -> Expectation
desugarsTo test expected = do
    (s, res) <- dropLogs $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right x <- Pass.eval' test
        setAttr (typeVal' @UniqueNameGen) ("obscureName", 0)
        setAttr (typeVal' @UsedVars) []
        Right desugared <- Pass.eval' $ desugar $ generalize x
        Right s <- Pass.eval' $ printExpr desugared
        Right expected <- Pass.eval' $ expected
        Right result <- Pass.eval' $ areExpressionsIsomorphic @(SubPass TestPass _) (unsafeRelayout expected) (unsafeRelayout desugared)
        return (s, result)
    putStrLn s
    res `shouldBe` True

-- replacesTo :: _ => _ -> _ -> Expectation
-- replacesTo test expected = do
--     res <- dropLogs $ evalIRBuilder' $ evalPassManager' $ do
--         runRegs
--         Right x <- Pass.eval' test
--         setAttr (typeVal' @UniqueNameGen) ("obscureName", 0)
--         Right desugared <- fst <$> Pass.eval' $ runWriterT $ replaceBlanks $ generalize x
--         Right expected' <- Pass.eval' $ expected
--         Right result <- Pass.eval' $ areExpressionsIsomorphic @(SubPass TestPass _) (unsafeRelayout expected') (unsafeRelayout desugared)
--         return result
--     res `shouldBe` True

prints :: _ => _ -> Expectation
prints test = do
    s <- dropLogs $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        Right x <- Pass.eval' test
        setAttr (typeVal' @UniqueNameGen) ("obscureName", 0)
        setAttr (typeVal' @UsedVars) []
        Right s <- Pass.eval' $ printExpr $ generalize x
        return s
    putStrLn s

fooBlank7 :: _ => SubPass TestPass m _
fooBlank7 = do
    foo <- strVar "foo"
    b <- blank
    seven <- integer 7
    app1 <- app b (arg seven)
    app foo (arg app1)

fooBlank7Replaced :: _ => SubPass TestPass m _
fooBlank7Replaced = do
    foo <- strVar "foo"
    v <- strVar obscureName
    seven <- integer 7
    app1 <- app v (arg seven)
    app foo (arg app1)

fooBlank7Expected :: _ => SubPass TestPass m _
fooBlank7Expected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    seven <- integer 7
    app1 <- app v (arg seven)
    app2 <- app foo (arg app1)
    lam (arg v) app2

blank1 :: _ => SubPass TestPass m _
blank1 = do
    b <- blank
    one <- integer 1
    app b (arg one)

blank1Expected :: _ => SubPass TestPass m _
blank1Expected = do
    v <- strVar obscureName
    one <- integer 1
    a <- app v (arg one)
    lam (arg v) a

fooBlank :: _ => SubPass TestPass m _
fooBlank = do
    foo <- strVar "foo"
    b <- blank
    app foo (arg b)

fooBlankReplaced :: _ => SubPass TestPass m _
fooBlankReplaced = do
    foo <- strVar "foo"
    v <- strVar obscureName
    app foo (arg v)

fooBlankExpected :: _ => SubPass TestPass m _
fooBlankExpected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    a <- app foo (arg v)
    lam (arg v) a

fooBlankGrouped7 :: _ => SubPass TestPass m _
fooBlankGrouped7 = do
    foo <- strVar "foo"
    b <- blank
    a <- app foo (arg b)
    g <- grouped a
    seven <- integer 7
    app g (arg seven)

fooBlankGrouped7Expected :: _ => SubPass TestPass m _
fooBlankGrouped7Expected = do
    foo <- strVar "foo"
    v <- strVar obscureName
    a <- app foo (arg v)
    l <- lam (arg v) a
    g <- grouped l
    seven <- integer 7
    app g (arg seven)

blankDotFooBlankBar :: _ => SubPass TestPass m _
blankDotFooBlankBar = do
    bDotFoo <- blankDotFoo
    b <- blank
    bar <- strVar "bar"
    a <- app b (arg bar)
    app bDotFoo (arg a)

blankDotFooBlankBarReplaced :: _ => SubPass TestPass m _
blankDotFooBlankBarReplaced = do
    v0 <- strVar obscureName
    vDotFoo <- rawAcc "foo" v0
    v1 <- strVar "#obscureName1"
    bar <- strVar "bar"
    a <- app v1 (arg bar)
    app vDotFoo (arg a)

blankDotFooBlankBarExpected :: _ => SubPass TestPass m _
blankDotFooBlankBarExpected = do
    v0 <- strVar obscureName
    v1 <- strVar "#obscureName1"
    bar <- strVar "bar"
    a <- app v1 (arg bar)
    ac <- rawAcc "foo" v0
    a' <- app ac (arg a)
    l <- lam (arg v1) a'
    lam (arg v0) l

blankDotFooGroupedBlankBar :: _ => SubPass TestPass m _
blankDotFooGroupedBlankBar = do
    bDotFoo <- blankDotFoo
    b <- blank
    bar <- strVar "bar"
    a <- app b (arg bar)
    g <- grouped bDotFoo
    app g (arg a)

blankDotFooGroupedBlankBarExpected :: _ => SubPass TestPass m _
blankDotFooGroupedBlankBarExpected = do
    v0 <- strVar obscureName
    v1 <- strVar "#obscureName1"
    bar <- strVar "bar"
    a <- app v1 (arg bar)
    ac <- rawAcc "foo" v0
    l <- lam (arg v0) ac
    a' <- app l (arg a)
    lam (arg v1) a'

fooBarBlankBaz :: _ => SubPass TestPass m _
fooBarBlankBaz = do
    foo <- strVar "foo"
    bar <- strVar "bar"
    baz <- strVar "baz"
    b <- blank
    a <- app b (arg baz)
    a' <- app bar (arg a)
    app foo (arg a')

fooBarBlankBazExpected :: _ => SubPass TestPass m _
fooBarBlankBazExpected = do
    v0 <- strVar obscureName
    foo <- strVar "foo"
    bar <- strVar "bar"
    baz <- strVar "baz"
    a <- app v0 (arg baz)
    a' <- app bar (arg a)
    a'' <- app foo (arg a')
    lam (arg v0) a''

spec :: Spec
spec = do
  -- describe "replace blanks" $ do
  --   it "foo _ => foo #obscureName0" $ fooBlank `replacesTo` fooBlankReplaced
  --   it "foo _ 7 => foo #obscureName0 7" $ fooBlank7 `replacesTo` fooBlank7Replaced
  --   it "_.foo _ bar => #obscureName0.foo #obscureName1 bar" $ blankDotFooBlankBar `replacesTo` blankDotFooBlankBarReplaced
  describe "blank desugaring" $ do
    it "_.foo => (\\x -> x.foo)" $ do
      prints blankDotFoo
      prints blankDotFooExpected
      blankDotFoo `desugarsTo` blankDotFooExpected
    it "_ 1 => (\\x -> x 1)" $ do
      prints blank1
      prints blank1Expected
      blank1 `desugarsTo` blank1Expected
    it "foo _ => (\\x -> foo x)" $ do
      prints fooBlank
      prints fooBlankExpected
      fooBlank `desugarsTo` fooBlankExpected
    it "foo _ 7 => (\\x -> foo x 7)" $ do
      prints fooBlank7
      prints fooBlank7Expected
      fooBlank7 `desugarsTo` fooBlank7Expected
    it "(foo _) 7 => (\\x -> foo x) 7" $ do
      prints fooBlankGrouped7
      prints fooBlankGrouped7Expected
      fooBlankGrouped7 `desugarsTo` fooBlankGrouped7Expected
    it "_.foo _ bar => (\\x y -> x.foo y bar)" $ do
      prints blankDotFooBlankBar
      prints blankDotFooBlankBarExpected
      blankDotFooBlankBar `desugarsTo` blankDotFooBlankBarExpected
    it "(_.foo) _ bar => (\\y -> (\\x -> x.foo) y bar)" $ do
      prints blankDotFooGroupedBlankBar
      prints blankDotFooGroupedBlankBarExpected
      blankDotFooGroupedBlankBar `desugarsTo` blankDotFooGroupedBlankBarExpected
    it "foo bar _ baz => (\\x -> foo bar x baz)" $ do
      prints fooBarBlankBaz
      prints fooBarBlankBazExpected
      fooBarBlankBaz `desugarsTo` fooBarBlankBazExpected
