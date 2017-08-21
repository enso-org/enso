{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Compilation.FlowSpec where

import           Luna.Prelude        hiding (String, seq, cons)
import qualified Luna.Prelude        as P
import qualified OCI.Pass           as Pass
import           OCI.Pass           (SubPass)
import qualified OCI.IR.Repr.Vis    as Vis
import qualified Data.Map            as Map
import           System.IO.Unsafe  (unsafePerformIO)

import Luna.Test.IR.Runner
import Luna.IR
import Luna.Builtin.Data.Module
import Luna.Test.Utils
import OCI.IR.Combinators
import System.Log
import Control.Monad.Raise
import qualified Control.Monad.State as State

import Luna.Pass.Data.ExprRoots
import Luna.Pass.Data.UniqueNameGen
import Luna.Pass.Resolution.Data.UnresolvedVars
import Luna.Pass.Resolution.Data.CurrentTarget
import Luna.Pass.Inference.Data.SimplifierQueue
import Luna.Pass.Inference.Data.Unifications

import qualified Luna.Pass.Transform.Desugaring.BlankArguments as BlankDesugaring
import qualified Luna.Pass.Transform.Desugaring.RemoveGrouped  as RemoveGrouped

import qualified Luna.Pass.Resolution.AliasAnalysis      as AliasAnalysis
import qualified Luna.Pass.Resolution.FunctionResolution as FunctionResolution

import qualified Luna.Pass.Inference.StructuralTyping   as StructuralTyping
import qualified Luna.Pass.Inference.TypeSimplification as Simplification
import qualified Luna.Pass.Inference.UnificationSolver  as UniSolver

import Luna.Pass.Typechecking.Typecheck (typecheck)

import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldSatisfy, Expectation)
import Data.TypeDesc

import Luna.Builtin.Std hiding (LTp (..))

infixl 2 :.:
infixr 3 :$:
infixr 4 :->:

data LType = LType :->: LType
           | LType :.:  Name
           | LType :$:  LType
           | LCons Name [LType]
           | LVar Name
           | LNumber
           | LStar
           deriving (Show, Eq)

repType :: forall m. (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass TestPass m LType
repType expr = getLayer @Type expr >>= source >>= go where
    go :: Expr Draft -> SubPass TestPass m LType
    go expr = matchExpr expr $ \case
        Monadic s _ -> go =<< source s
        Var  n      -> return $ LVar n
        Cons n as   -> LCons n <$> mapM (source >=> go) as
        Lam  i o    -> (:->:)  <$> (source i >>= go) <*> (source o >>= go)
        Acc  t n    -> (:.:)   <$> (source t >>= go) <*> pure n
        App  f a    -> (:$:)   <$> (source f >>= go) <*> (source a >>= go)
        Star        -> return LStar
        Number{}    -> return LNumber
        x           -> return LStar--error $ "unexpected node: " ++ show x

data LMonad = LPure | LIO | LUnknown | LMalformed deriving (Ord, Show, Eq)

instance Mempty LMonad where
    mempty = LPure

instance Semigroup LMonad where
    (<>) = max

repMonad :: forall m. (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass TestPass m LMonad
repMonad expr = getLayer @Type expr >>= source >>= go where
    gatherMonads e = matchExpr e $ \case
        Cons n _  -> return $ if n == "IO" then LIO else if n == "Pure" then LPure else LUnknown
        Unify l r -> (<>) <$> (gatherMonads =<< source l) <*> (gatherMonads =<< source r)
        _         -> return LUnknown
    go e = matchExpr e $ \case
        Monadic _ m -> gatherMonads =<< source m
        _           -> return LMalformed

subtreeErrors :: forall m. (MonadRef m, MonadPassManager m) => Expr Draft -> SubPass TestPass m [CompileError]
subtreeErrors e = do
    locErr <- getLayer @Errors e
    recErr <- mapM subtreeErrors =<< mapM source =<< inputs e
    return $ concat $ locErr : recErr

isSolved :: LType -> Bool
isSolved (a :->: b)   = isSolved a && isSolved b
isSolved (_ :.:  _)   = False
isSolved (_ :$:  _)   = False
isSolved (LCons _ as) = all isSolved as
isSolved (LVar _)     = True
isSolved LStar        = False
isSolved LNumber      = True


eqUptoVarNames :: LType -> LType -> Bool
eqUptoVarNames = flip State.evalState (Map.empty, Map.empty) .: go where
    go (a :->: b)   (a' :->: b')   = (&&) <$> go a a' <*> go b b'
    go (a :.:  n)   (a' :.:  n')   = ((n == n') &&) <$> go a a'
    go (a :$:  b)   (a' :$:  b')   = (&&) <$> go a a' <*> go b b'
    go (LCons n as) (LCons n' as') = ((n == n') &&) . and <$> zipWithM go as as'
    go LStar        LStar          = return True
    go LNumber      LNumber        = return True
    go (LVar a)     (LVar b)       = State.get >>= \(atob, btoa) -> case (Map.lookup a atob, Map.lookup b btoa) of
        (Just x,  Just y)  -> return $ x == b && y == a
        (Nothing, Nothing) -> State.modify (over _1 (Map.insert a b) . over _2 (Map.insert b a)) >> return True
        (_,       _)       -> return False
    go _ _ = return False

runTC :: Imports -> Bool -> SubPass TestPass (PMStack IO) ([Expr Draft], Expr Draft) -> IO (LType, LMonad, [CompileError])
runTC imports vis c = do
    Right res <- runPM True $ do
        runRegs
        (roots, root) <- Pass.eval' @TestPass c
        when vis $ Pass.eval' @TestPass $ Vis.snapshot "start"
        trans <- typecheck TgtNone imports roots
        when vis $ Pass.eval' @TestPass $ Vis.snapshot "end"
        let newRoot = maybe root id $ Map.lookup root trans
        tp <- Pass.eval' $ repType newRoot
        er <- Pass.eval' $ subtreeErrors newRoot
        mo <- Pass.eval' $ repMonad newRoot
        return (tp, mo, er)
    return res

infix 1 `shouldBeEquivalentTo`
shouldBeEquivalentTo :: (LType, LMonad, [Text]) -> LType -> Expectation
shouldBeEquivalentTo (a, _, e) b = do
    length e `shouldBe` 0
    a `shouldSatisfy` (eqUptoVarNames b)

infix 1 `shouldBeInMonad`
shouldBeInMonad :: (LType, LMonad, [Text]) -> LMonad -> Expectation
shouldBeInMonad (_, m, _) mo = m `shouldBe` mo

{-std :: Imports-}
{-std = unsafePerformIO $ snd <$> mockStdlib-}
{-{-# NOINLINE std #-}-}

{-std :: Imports-}
{-std = def-}

spec :: Spec
spec = do
    let int     = LCons "Int"    []
        string' = LCons "String" []
        maybe a = LCons "Maybe"  [a]
        num     = LNumber
        a       = LVar "a"
        b       = LVar "b"
    -- in all examples the `{}` notation stands for Grouped expression
    describe "typechecking flow" $ it "works" $ pending
        {-it "x = plus _ 7" $ do-}
            {-result <- runTC std False $ do-}
                {-plus  <- var "plus"-}
                {-bl    <- blank-}
                {-seven <- number 7-}
                {-apbl  <- app plus bl-}
                {-apse  <- app apbl seven-}
                {-x     <- var "x"-}
                {-uni   <- unify x apse-}
                {-return ([generalize uni], generalize uni)-}
            {-result `shouldBeEquivalentTo` int :->: int-}
        {-it "a: a" $ do-}
            {-result <- runTC std False $ do-}
                {-a <- var "a"-}
                {-l <- lam a a-}
                {-return ([generalize l], generalize l)-}
            {-result `shouldBeEquivalentTo` a :->: a-}
        {-it "a: ({plus _} a) 1" $ do-}
            {-result <- runTC std False $ do-}
                {-a  <- var "a"-}
                {-a' <- var "a"-}
                {-i1 <- number 1-}
                {-pl <- var "plus"-}
                {-bl <- blank-}
                {-f  <- app pl bl-}
                {-gf <- grouped f-}
                {-a1 <- app gf a-}
                {-a2 <- app a1 i1-}
                {-l  <- lam a' a2-}
                {-return ([generalize l], generalize l)-}
            {-result `shouldBeEquivalentTo` int :->: int-}
        {-it "b = intId" $ do-}
            {-result <- runTC std False $ do-}
                {-b     <- var "b"-}
                {-intId <- var "__intId"-}
                {-root  <- unify b intId-}
                {-return ([generalize root], generalize root)-}
            {-result `shouldBeEquivalentTo` int :->: int-}
        {-it "a = id; b = id a" $ do-}
            {-result <- runTC std False $ do-}
                {-a      <- var "a"-}
                {-intId  <- var "id"-}
                {-uni    <- unify a intId-}

                {-b      <- var "b"-}
                {-intId' <- var "id"-}
                {-apid   <- app intId' a-}
                {-uni'   <- unify b apid-}

                {-s      <- seq uni uni'-}

                {-return ([generalize s], generalize s)-}
            {-result `shouldBeEquivalentTo` a :->: a-}
        {-it "x: Just (plus x 1)" $ do-}
            {-result <- runTC std False $ do-}
                {-x          <- var "x"-}
                {-plus       <- var "plus"-}
                {-one        <- number 1-}
                {-plus_x     <- app plus x-}
                {-plus_x_one <- app plus_x one-}
                {-just       <- cons "Just" [plus_x_one]-}
                {-l          <- generalize <$> lam x just-}
                {-return ([l], l)-}
            {-result `shouldBeEquivalentTo` int :->: maybe int-}
        {-it "(Just x) : plus x 1" $ do-}
            {-result <- runTC std False $ do-}
                {-x        <- var "x"-}
                {-just_x   <- cons "Just" [x]-}
                {-plus     <- var "plus"-}
                {-plus_x   <- app plus x-}
                {-one      <- number 1-}
                {-plus_x_1 <- app plus_x one-}
                {-lambda   <- generalize <$> lam just_x plus_x_1-}
                {-return ([lambda], lambda)-}
            {-result `shouldBeEquivalentTo` maybe int :->: int-}
        {-it (unlines [ "x : case x:"-}
                    {-, "  Just x  -> plus x 1"-}
                    {-, "  Nothing -> 0"-}
                    {-]) $ do-}
            {-result <- runTC std False $ do-}
                {-x            <- var "x"-}
                {-xinJ         <- var "x"-}
                {-plus         <- var "plus"-}
                {-one          <- number 1-}
                {-just_xinJ    <- cons "Just" [xinJ]-}
                {-plus_xinJ    <- app plus xinJ-}
                {-plus_xinJ_1  <- app plus_xinJ one-}
                {-zero         <- number 0-}
                {-nothing      <- cons_ @Draft "Nothing"-}
                {-cl1          <- clause just_xinJ plus_xinJ_1-}
                {-cl2          <- clause nothing   zero-}
                {-pat          <- match  x [generalize cl1 :: Expr Draft, generalize cl2]-}
                {-l            <- generalize <$> lam x pat-}
                {-return ([l], l)-}
            {-result `shouldBeEquivalentTo` maybe int :->: int-}
        {-it "foo = x y: x ; bar = 532 ; baz = foo bar" $ do-}
            {-result <- runTC std False $ do-}
                {-x      <- var "x"-}
                {-y      <- var "y"-}
                {-l1     <- lam y x-}
                {-l2     <- lam x l1-}
                {-foo    <- var "foo"-}
                {-unifoo <- unify foo l2-}

                {-bar    <- var "bar"-}
                {-num    <- number 532-}
                {-unibar <- unify bar num-}

                {-baz    <- var "baz"-}
                {-bazbod <- app foo bar-}
                {-unibaz <- unify baz bazbod-}

                {-s1     <- seq unifoo unibar-}
                {-s2     <- seq s1     unibaz-}

                {-return ([generalize s2], generalize baz)-}
            {-result `shouldBeEquivalentTo` a :->: int-}
        {-it "1.+ 3" $ do-}
            {-result <- runTC std False $ do-}
                {-one   <- number 1-}
                {-three <- number 3-}
                {-onepl <- acc one "+"-}
                {-res   <- app onepl three-}
                {-return ([generalize res], generalize res)-}
            {-result `shouldBeEquivalentTo` int-}
        {-it "+" $ do-}
            {-result <- runTC std False $ do-}
                {-plus <- var "+"-}
                {-return ([generalize plus], generalize plus)-}
            {-result `shouldBeEquivalentTo` a :->: a :->: a-}
        {-it "1 + 3" $ do-}
            {-result <- runTC std False $ do-}
                {-one   <- number 1-}
                {-three <- number 3-}
                {-plus  <- var "+"-}
                {-onepl <- app plus one-}
                {-res   <- app onepl three-}
                {-return ([generalize res], generalize res)-}
            {-result `shouldBeEquivalentTo` int-}
        {-it "(1 +)" $ do-}
            {-result <- runTC std False $ do-}
                {-one  <- number 1-}
                {-plus <- var "+"-}
                {-appl <- app plus one-}
                {-return ([generalize appl], generalize appl)-}
            {-result `shouldBeEquivalentTo` int :->: int-}
        {-it "(\"str\" +)" $ do-}
            {-result <- runTC std False $ do-}
                {-str  <- string "str"-}
                {-plus <- var "+"-}
                {-appl <- app plus str-}
                {-return ([generalize appl], generalize appl)-}
            {-result `shouldBeEquivalentTo` string' :->: string'-}
        {-it "x = Just 5 ; x.shortRep" $ do-}
            {-result <- runTC std False $ do-}
                {-x     <- var "x"-}
                {-five  <- number 5-}
                {-just  <- cons_ @Draft "Just"-}
                {-just5 <- app just five-}
                {-uni   <- unify x just5-}
                {-shr   <- acc x "shortRep"-}
                {-s     <- seq uni shr-}
                {-return ([generalize s], generalize s)-}
            {-result `shouldBeEquivalentTo` string'-}
        {-it "x = Empty ; x.fold 0 (+)" $ do-}
            {-result <- runTC std False $ do-}
                {-x     <- var "x"-}
                {-empty <- cons_ @Draft "Empty"-}
                {-uni   <- unify x empty-}
                {-fold  <- acc x "fold"-}
                {-zero  <- number 0-}
                {-plus  <- var "+"-}
                {-ap1   <- app fold zero-}
                {-ap2   <- app ap1  plus-}
                {-s     <- seq uni ap2-}
                {-return ([generalize s], generalize s)-}
            {-result `shouldBeEquivalentTo` int-}
    {-describe "monad solver" $ do-}
        {-it "1 + 1" $ do-}
            {-result <- runTC std False $ do-}
                {-plus <- var "+"-}
                {-one  <- number 1-}
                {-one' <- number 1-}
                {-ap1  <- app plus one-}
                {-ap2  <- app ap1  one'-}
                {-return ([generalize ap2], generalize ap2)-}
            {-result `shouldBeInMonad` LPure-}
        {-it "testPrint 10" $ do-}
            {-result <- runTC std False $ do-}
                {-print <- var "testPrint"-}
                {-ten   <- number 10-}
                {-ap    <- app print ten-}
                {-return ([generalize ap], generalize ap)-}
            {-result `shouldBeInMonad` LIO-}
        {-it "next + 1" $ do-}
            {-result <- runTC std False $ do-}
                {-plus <- var "+"-}
                {-one  <- number 1-}
                {-next <- var "next"-}
                {-ap1  <- app plus next-}
                {-ap2  <- app ap1  one-}
                {-return ([generalize ap2], generalize ap2)-}
            {-result `shouldBeInMonad` LIO-}
        {-it "foo = x: y: x ; foo next \"hello\"" $ do-}
            {-result <- runTC std False $ do-}
                {-foo <- var "foo"-}
                {-x   <- var "x"-}
                {-y   <- var "y"-}
                {-l1  <- lam y x-}
                {-l2  <- lam x l1-}
                {-u   <- unify foo l2-}

                {-next  <- var "next"-}
                {-hello <- string "hello"-}
                {-ap1   <- app foo next-}
                {-ap2   <- app ap1 hello-}

                {-s <- seq u ap2-}

                {-return ([generalize s], generalize s)-}
            {-result `shouldBeInMonad` LIO-}
        {-it "foo = x: y: x ; foo \"hello\" next" $ do-}
            {-result <- runTC std False $ do-}
                {-foo <- var "foo"-}
                {-x   <- var "x"-}
                {-y   <- var "y"-}
                {-l1  <- lam y x-}
                {-l2  <- lam x l1-}
                {-u   <- unify foo l2-}

                {-next  <- var "next"-}
                {-hello <- string "hello"-}
                {-ap1   <- app foo hello-}
                {-ap2   <- app ap1 next-}

                {-s <- seq u ap2-}

                {-return ([generalize s], generalize s)-}
            {-result `shouldBeInMonad` LPure-}
