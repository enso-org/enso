{-# LANGUAGE PartialTypeSignatures #-}

module Luna.Pass.StructuralTypingSpec where

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

data StructuralTyping
type instance Abstract StructuralTyping = StructuralTyping
type instance Inputs  Net   StructuralTyping = '[AnyExpr, AnyExprLink]
type instance Outputs Net   StructuralTyping = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer StructuralTyping = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // Succs, AnyExprLink // UID, AnyExprLink // Model]
type instance Outputs Layer StructuralTyping = '[AnyExpr // Model, AnyExpr // UID, AnyExpr // Type, AnyExpr // Succs, AnyExprLink // UID, AnyExprLink // Model]
type instance Inputs  Attr  StructuralTyping = '[CurrentRoot, FreshVars]
type instance Outputs Attr  StructuralTyping = '[FreshVars]
type instance Inputs  Event StructuralTyping = '[]
type instance Outputs Event StructuralTyping = '[New // AnyExpr, New // AnyExprLink, Delete // AnyExpr, Delete // AnyExprLink]
type instance Preserves     StructuralTyping = '[]

data    FreshVars   = FreshVars P.String Int
newtype CurrentRoot = CurrentRoot SomeExpr
makeWrapped ''CurrentRoot

freshName :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass StructuralTyping m P.String
freshName = do
    FreshVars base c <- readAttr @FreshVars
    writeAttr @FreshVars $ FreshVars base $ succ c
    return $ base <> show c

getStructuralType :: (MonadRef m, MonadIO m, MonadPassManager m) => SomeExpr -> SubPass StructuralTyping m SomeExpr
getStructuralType expr = do
    tp <- readLayer @Type expr >>= source
    match tp $ \case
        Star -> attachStructuralType expr
        _    -> return tp

attachStructuralType :: (MonadRef m, MonadIO m, MonadPassManager m) => SomeExpr -> SubPass StructuralTyping m SomeExpr
attachStructuralType expr = do
    tp <- match expr $ \case
        Integer {}      -> string "Int"    >>= fmap generalize . cons_
        Rational{}      -> string "Double" >>= fmap generalize . cons_ --TODO type as rational
        String  {}      -> string "String" >>= fmap generalize . cons_
        Acc n a         -> do
            name <- source n
            source a >>= getStructuralType >>= fmap generalize . acc name
        App f (Arg n a) -> do
            argTp <- getStructuralType =<< source a
            fTp   <- getStructuralType =<< source f
            generalize <$> app fTp (Arg n argTp)
        Lam (Arg n a) o -> do
            argTp <- getStructuralType =<< source a
            outTp <- getStructuralType =<< source o
            generalize <$> lam (Arg n argTp) outTp
        Unify a b -> do
            getStructuralType =<< source a
            getStructuralType =<< source b
            fmap generalize $ freshName >>= string >>= var
        Grouped a -> do
            getStructuralType =<< source a
            fmap generalize $ freshName >>= string >>= var
        _ -> fmap generalize $ freshName >>= string >>= var
    tpl   <- readLayer @Type expr
    oldTp <- source tpl
    changeSource tpl tp
    deleteSubtree oldTp
    return tp

assignStructuralType :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass StructuralTyping m ()
assignStructuralType = (unwrap <$> readAttr @CurrentRoot) >>= getStructuralType >> return ()

-- test1 :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TestPass m SomeExpr
test1 = do
    vfoo <- strVar "foo"
    bar  <- string "bar"
    vbaz <- strVar "baz"
    ac   <- acc bar vfoo
    ap   <- app ac (arg vbaz)
    return $ unsafeGeneralize ap

-- result1 :: (MonadRef m, MonadIO m, MonadPassManager m) => SubPass TestPass m SomeExpr
result1 = do
    a0 <- strVar "a0"
    a1 <- strVar "a1"
    bar <- string "bar"
    ac  <- acc bar a1
    ap  <- app ac (arg a0)
    return $ unsafeGeneralize ap

typesAs :: _ => _ -> _ -> Expectation
typesAs test expect = do
    Right (res, coh) <- tryAll $ dropLogs $ runRefCache $ evalIRBuilder' $ evalPassManager' $ do
        runRegs
        (root :: SomeExpr) <- Pass.eval' @TestPass test
        setAttr (getTypeDesc @FreshVars)   $ FreshVars "a" 0
        setAttr (getTypeDesc @CurrentRoot) $ root
        Pass.eval' assignStructuralType
        resTp  <- Pass.eval' @TestPass $ fmap unsafeGeneralize $ readLayer @Type root >>= source
        exRoot <- Pass.eval' @TestPass expect
        res    <- Pass.eval' @TestPass $ areExpressionsIsomorphic resTp exRoot
        coh    <- Pass.eval' @TestPass $ checkCoherence
        return (res, coh)
    res `shouldBe` True
    coh `shouldBe` []

spec = do
    describe "foo.bar baz" $
        it "types correctly" $ test1 `typesAs` result1
