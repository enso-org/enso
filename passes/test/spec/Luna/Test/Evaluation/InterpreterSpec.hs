{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Evaluation.InterpreterSpec where

import Prelude (read)
import Luna.Prelude   as P hiding (seq, force, Constructor, cons, Text)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ratio (numerator)
import Data.Maybe (fromJust)

import           Luna.IR hiding (get, put)
import           Luna.IR.Term.Literal (intPart)
import           OCI.Pass (SubPass, Inputs, Outputs, Preserves, Events)
import qualified OCI.Pass        as Pass
import           Control.Monad.Trans.State.Lazy (StateT, runStateT, evalStateT, get, gets, put, modify)

import           Luna.Builtin.Data.Function (compile, useCompiled)
import           Luna.Builtin.Prim

import Luna.Pass.Evaluation.Interpreter
import Luna.Builtin.Data.LunaValue
import Luna.Builtin.Data.LunaEff
import Luna.Builtin.Data.Module
import Luna.Builtin.Data.Class
import Luna.Builtin.Std

import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldSatisfy, Expectation, expectationFailure)
import System.Log
import Luna.Test.Utils
import Control.Monad.Raise
import Luna.Test.IR.Runner
import Luna.Test.Utils
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef, IORef)
import System.IO.Unsafe

import Control.Monad.Except
import Data.Text.Lazy (Text)

{-getStdout :: WorldState -> IO [Int]-}
{-getStdout = fmap reverse . readIORef . view stdout-}

runInt :: SubPass Interpreter (PMStack IO) a -> IO a
runInt m = do
    Right res <- runPM False $ runRegs >> Pass.eval' m
    return res

{-std :: (WorldState, Imports)-}
{-std = unsafePerformIO mockStdlib-}
{-{-# NOINLINE std #-}-}

{-runTest' :: SubPass TestPass (PMStack IO) (Expr Draft) -> IO (WorldState, LunaValue)-}
{-runTest' c = do-}
    {-let (world, glob) = std-}
    {-resetWorld world-}
    {-Right val <- runPM False $ do-}
        {-runRegs-}
        {-root <- Pass.eval' c-}
        {-Pass.eval' $ interpret glob root-}
    {-return (world, evalStateT val def)-}

{-runTest :: SubPass TestPass (PMStack IO) (Expr Draft) -> IO LunaValue-}
{-runTest = fmap snd . runTest'-}

shouldBeLunaString :: LunaData -> Text -> Expectation
shouldBeLunaString (LunaBoxed a) b = (fromBoxed a) `shouldBe` b
shouldBeLunaString (LunaThunk a) b = do
    new <- runIO $ runError a
    case new of
        Left err -> expectationFailure $ "Expected LunaInt, got error: " ++ err
        Right n  -> n `shouldBeLunaString` b
shouldBeLunaString _             _ = expectationFailure "Expected a LunaString"

shouldReturnLunaString :: LunaValue -> Text -> Expectation
shouldReturnLunaString v s = LunaThunk v `shouldBeLunaString` s

shouldBeLunaInt :: LunaData -> Int -> Expectation
shouldBeLunaInt (LunaBoxed a) b = (fromBoxed a) `shouldBe` b
shouldBeLunaInt (LunaThunk a) b = do
    new <- runIO $ runError a
    case new of
        Left err -> expectationFailure $ "Expected LunaInt, got error: " ++ err
        Right n  -> n `shouldBeLunaInt` b
shouldBeLunaInt _             _ = expectationFailure "Expected a LunaInt"

shouldReturnLunaInt :: LunaValue -> Int -> Expectation
shouldReturnLunaInt v i = LunaThunk v `shouldBeLunaInt` i

printingFact :: SubPass TestPass (PMStack IO) (Expr Var, Expr Unify)
printingFact = do
    zero <- number 0
    one  <- number 1
    fact <- var "fact"
    n    <- var "n"
    eq   <- var "eq"
    if'  <- var "if_then_else"
    prt  <- var "testPrint"

    eq_n         <- app eq n
    eq_n_0       <- app eq_n zero
    pred_n       <- acc n "pred"
    fact_pred_n  <- app fact pred_n
    mult_n       <- acc n "*"
    mult_n_fact  <- app mult_n fact_pred_n
    if_eq        <- app if' eq_n_0
    if_eq_1      <- app if_eq one
    if_eq_1_mult <- app if_eq_1 mult_n_fact

    print_n      <- app prt n
    prt_if       <- seq print_n if_eq_1_mult
    factorial    <- lam n prt_if
    uni          <- unify fact factorial
    return (generalize fact, generalize uni)

spec :: Spec
spec = describe "interpreter" $ it "works" $ pending
    {-describe "non-uni expressions" $ do-}
        {-it "x = Just 5 ; x.shortRep" $ do-}
            {-resVal <- runTest $ do-}
                {-x     <- var "x"-}
                {-five  <- number 5-}
                {-just5 <- cons "Just" [five]-}
                {-uni   <- unify x just5-}
                {-shr   <- acc x "shortRep"-}
                {-s     <- seq uni shr-}
                {-return $ generalize s-}
            {-resVal `shouldReturnLunaString` "Just 5"-}
        {-it "((x: x) 3).+ ((y: y) 4)" $ do-}
            {-resVal <- runTest $ do-}
                {-x     <- var "x"-}
                {-idx   <- lam x x-}
                {-three <- number 3-}
                {-apidx <- app idx three-}
                {-y     <- var "y"-}
                {-idy   <- lam y y-}
                {-four  <- number 4-}
                {-apidy <- app idy four-}
                {-appl  <- acc apidx "+"-}
                {-appl' <- app appl apidy-}
                {-return $ generalize appl'-}
            {-resVal `shouldReturnLunaInt` 7-}
    {-describe "pure uni expressions" $ do-}
        {-it "x = 1.+ 2; x.+ 3" $ do-}
            {-resVal <- runTest $ do-}
                {-x    <- var "x"-}
                {-one  <- number 1-}
                {-two  <- number 2-}
                {-ap1  <- acc one "+"-}
                {-ap2  <- app ap1 two-}
                {-uni  <- unify x ap2-}

                {-three <- number 3-}
                {-ap3   <- acc x "+"-}
                {-ap4   <- app ap3 three-}

                {-generalize <$> seq uni ap4-}
            {-resVal `shouldReturnLunaInt` 6-}
        {-it "x = Empty ; x.fold 0 (+)" $ do-}
            {-resVal <- runTest $ do-}
                {-x     <- var "x"-}
                {-empty <- cons_ @Draft "Empty"-}
                {-uni   <- unify x empty-}
                {-fold  <- acc x "fold"-}
                {-zero  <- number 0-}
                {-plus  <- var "+"-}
                {-ap1   <- app fold zero-}
                {-ap2   <- app ap1  plus-}
                {-generalize <$> seq uni ap2-}
            {-resVal `shouldReturnLunaInt` 0-}
        {-it "x = Empty ; x.shortRep" $ do-}
            {-resVal <- runTest $ do-}
                {-x     <- var "x"-}
                {-empty <- cons_ @Draft "Empty"-}
                {-uni   <- unify x empty-}
                {-shr   <- acc x "shortRep"-}
                {-generalize <$> seq uni shr-}
            {-resVal `shouldReturnLunaString` "List<0>"-}
        {-it "x = Empty ; x.toJSON" $ do-}
            {-resVal <- runTest $ do-}
                {-x     <- var "x"-}
                {-empty <- cons_ @Draft "Empty"-}
                {-uni   <- unify x empty-}
                {-shr   <- acc x "toJSON"-}
                {-generalize <$> seq uni shr-}
            {-resVal `shouldReturnLunaString` "[]"-}
        {-it "x = Empty . prepend 20 . prepend 0 ; y = x.filter (eq 0) ; y . toJSON" $ do-}
            {-resVal <- runTest $ do-}
                {-x     <- var "x"-}
                {-twent <- number 20-}
                {-zero  <- number 0-}
                {-empty <- cons_ @Draft "Empty"-}
                {-prp20 <- flip app twent =<< acc empty "prepend"-}
                {-prp0  <- flip app zero  =<< acc prp20 "prepend"-}
                {-uniX  <- unify x prp0-}

                {-y       <- var "y"-}
                {-zero    <- number 0-}
                {-eq      <- var "eq"-}
                {-eq0     <- app eq zero-}
                {-filt    <- acc x "filter"-}
                {-filteq0 <- app filt eq0-}
                {-uniY    <- unify y filteq0-}

                {-shr   <- acc y "toJSON"-}

                {-seqXY <- seq uniX uniY-}

                {-generalize <$> seq seqXY shr-}
            {-resVal `shouldReturnLunaString` "[0]"-}
    {-describe "IO actions" $ do-}
        {-it "testPrint 0; testPrint 1; testPrint 2" $ do-}
            {-(world, resVal) <- runTest' $ do-}
                {-zero  <- number 0-}
                {-one   <- number 1-}
                {-two   <- number 2-}
                {-print <- var "testPrint"-}

                {-print_0 <- app print zero-}
                {-print_1 <- app print one-}
                {-print_2 <- app print two-}

                {-s1 <- seq print_0 print_1-}
                {-s2 <- seq s1 print_2-}
                {-return $ generalize s2-}
            {-runIO $ runError resVal-}
            {-stdout <- getStdout world-}
            {-stdout `shouldBe` [0, 1, 2]-}
        {-it "x = next; y = x; x.+ y" $ do-}
            {-resVal <- runTest $ do-}
                {-x    <- var "x"-}
                {-ne   <- var "next"-}
                {-uni1 <- unify x ne-}

                {-y    <- var "y"-}
                {-uni2 <- unify y x-}

                {-ap1  <- acc x "+"-}
                {-ap2  <- app ap1  y-}

                {-s1   <- seq uni1 uni2-}
                {-generalize <$> seq s1 ap2-}
            {-resVal `shouldReturnLunaInt` 2-}
        {-it "x = next; y = next; x.+ y" $ do-}
            {-resVal <- runTest $ do-}
                {-x    <- var "x"-}
                {-ne   <- var "next"-}
                {-uni1 <- unify x ne-}

                {-y    <- var "y"-}
                {-uni2 <- unify y ne-}

                {-ap1  <- acc x "+"-}
                {-ap2  <- app ap1  y-}

                {-s1   <- seq uni1 uni2-}
                {-generalize <$> seq s1 ap2-}
            {-resVal `shouldReturnLunaInt` 3-}
    {-describe "Laziness" $ do-}
        {-it "e = error 3; f = y x: y; f 3 e" $ do-}
            {-resVal <- runTest $ do-}
                {-e     <- var "e"-}
                {-err   <- var "error"-}
                {-three <- number 3-}
                {-aperr <- app err three-}
                {-uni1  <- unify e aperr-}

                {-y     <- var "y"-}
                {-x     <- var "x"-}
                {-l1    <- lam x y-}
                {-l2    <- lam y l1-}
                {-f     <- var "f"-}
                {-uni2  <- unify f l2-}

                {-apf3  <- app f three-}
                {-apf3e <- app apf3 e-}

                {-s1    <- seq uni1 uni2-}
                {-s2    <- seq s1   apf3e-}
                {-return $ generalize s2-}
            {-resVal `shouldReturnLunaInt` 3-}
    {-describe "Local recursion" $ do-}
        {-it "zero = z: if (eq z 0) 0 (zero z.pred)) ; zero 5" $ do-}
            {-resVal <- runTest $ do-}
                {-zero <- number 0-}
                {-eq   <- var "eq"-}
                {-if'  <- var "if_then_else"-}
                {-z    <- var "z"-}
                {-zvar <- var "zero"-}

                {-eq_z         <- app eq z-}
                {-eq_z_0       <- app eq_z zero-}
                {-pred_z       <- acc z "pred"-}
                {-zero_pred    <- app zvar pred_z-}
                {-if_eq        <- app if' eq_z_0-}
                {-if_eq_0      <- app if_eq zero-}
                {-if_eq_0_zero <- app if_eq_0 zero_pred-}
                {-l            <- lam z if_eq_0_zero-}
                {-uni          <- unify zvar l-}

                {-three <- number 5-}
                {-ap    <- app zvar three-}

                {-s <- seq uni ap-}
                {-return $ generalize s-}
            {-resVal `shouldReturnLunaInt` 0-}

        {-it "fact = n: if (eq n 0) 1 (n.* (fact n.pred)) ; fact 5" $ do-}
            {-resVal <- runTest $ do-}
                {-zero <- number 0-}
                {-one  <- number 1-}
                {-fact <- var "fact"-}
                {-n    <- var "n"-}
                {-eq   <- var "eq"-}
                {-mult <- var "mult"-}
                {-if'  <- var "if_then_else"-}

                {-eq_n         <- app eq n-}
                {-eq_n_0       <- app eq_n zero-}
                {-pred_n       <- acc n "pred"-}
                {-fact_pred_n  <- app fact pred_n-}
                {-mult_n       <- acc n "*"-}
                {-mult_n_fact  <- app mult_n fact_pred_n-}
                {-if_eq        <- app if' eq_n_0-}
                {-if_eq_1      <- app if_eq one-}
                {-if_eq_1_mult <- app if_eq_1 mult_n_fact-}

                {-factorial    <- lam n if_eq_1_mult-}
                {-uni          <- unify fact factorial-}

                {-five      <- number 5-}
                {-fact_five <- app fact five-}

                {-s <- seq uni fact_five-}
                {-return $ generalize s-}
            {-resVal `shouldReturnLunaInt` 120-}

        {-it "fact = (n: testPrint n ; if (eq n 0) 1 (n.* (fact n.pred))) ; fact 5" $ do-}
            {-(world, resVal) <- runTest' $ do-}
                {-(fact, uni) <- printingFact-}
                {-five        <- number 5-}
                {-fact_five   <- app fact five-}
                {-s           <- seq uni fact_five-}
                {-return $ generalize s-}
            {-resVal `shouldReturnLunaInt` 120-}
            {-stdout <- getStdout world-}
            {-stdout `shouldBe` [5, 4, 3, 2, 1, 0]-}
        {-it "fact = (n: testPrint n ; if (eq n 0) 1 (n.* (fact n.pred))) ; x = fact 5; x.+ x" $ do-}
            {-(world, resVal) <- runTest' $ do-}
                {-(fact, uni) <- printingFact-}
                {-five        <- number 5-}
                {-fact_five   <- app fact five-}
                {-x           <- var "x"-}
                {-u2          <- unify x fact_five-}
                {-x_plus      <- acc x "+"-}
                {-x_plus_x    <- app x_plus x-}
                {-s1          <- seq uni u2-}
                {-s2          <- seq s1  x_plus_x-}
                {-return $ generalize s2-}
            {-resVal `shouldReturnLunaInt` 240-}
            {-stdout <- getStdout world-}
            {-stdout `shouldBe` [5, 4, 3, 2, 1, 0]-}
