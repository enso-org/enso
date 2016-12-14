{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

module Luna.IR.PlaygroundSpec (spec) where

import Control.Monad (foldM, replicateM)
import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Luna.Prelude
import Luna.IR
import Luna.IR.Expr.Layout.Nested
import Luna.IR.Function
import Luna.IR.Runner
import Luna.IR.Expr.Layout.ENT
import qualified Luna.IR.Expr.Layout.Class as Layout
import           Old.Luna.Syntax.Model.Network.Builder  (Meta (..))
import qualified Data.HMap.Lazy                     as HMap


import qualified Luna.IR.Expr.Atom as Atom

import Test.Hspec hiding (Arg)

isLambda :: (IRMonad m, Readable (ExprLayer Model) m) => Expr l -> m Bool
isLambda expr = match expr $ \case
    Lam{} -> return True
    _     -> return False

isApp :: _ => Expr l -> m Bool
isApp expr = match expr $ \case
    App{} -> return True
    _     -> return False

isBlank :: _ => Expr layout0 -> m0 Bool
isBlank expr = match expr $ \case
    Blank{} -> return True
    _       -> return False

-- checksLambda :: SubPass TestPass (IRT IO) Bool
checksLambda = do
    b <- blank
    c <- blank
    foo <- lam (arg b) c
    isLambda foo

checksApp :: _
checksApp = do
    b <- blank
    c <- blank
    foo <- app b (arg c)
    isApp foo

checksBlank :: _
checksBlank = do
    b <- blank
    isBlank b

dumpArguments :: _ => Expr l -> m [Expr l]
dumpArguments expr = match expr $ \case
    App a (Arg _ b) -> do
        nextApp <- source a
        args    <- dumpArguments nextApp
        arg     <- source b
        return $ arg : args
    _       -> return []

checksArguments :: _
checksArguments = do
    v <- strVar "foo"
    b <- strVar "bar"
    a <- app v (arg b)
    dumpArguments (generalize a :: AnyExpr)

data NotAppException = NotAppException deriving (Show, Exception)

removeArg :: ( IRMonad m
             , Accessibles m '[ExprNet, ExprLinkNet]
             , Readables   m '[ExprLayer Model, ExprLinkLayer Model]
             , Emitter     m  (NEW // EXPR)
             , Emitter     m  (NEW // LINK' EXPR)
             , MonadThrow m
             ) => AnyExpr -> Int -> m AnyExpr
removeArg expr i = match expr $ \case
    App a (Arg _ c) -> do
        nextApp <- source a
        if i == 0 then do
            b  <- blank
            generalize <$> app nextApp (arg b)
        else do
            d <- source c
            f <- removeArg nextApp (i - 1)
            generalize <$> app f (arg d)
    _       -> throwM NotAppException

apps :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], Emitter m (NEW // EXPR), Emitter m (NEW // LINK' EXPR)) => Expr f -> [AnyExpr] -> m AnyExpr
apps fun exprs = unsafeRelayout <$> foldM f (unsafeRelayout fun) (unsafeRelayout <$> exprs)
    where
        f fun' arg' = appAny fun' (arg arg')

appAny :: (IRMonad m, Accessibles m '[ExprNet, ExprLinkNet], Emitter m (NEW // EXPR), Emitter m (NEW // LINK' EXPR))
          => AnyExpr -> Arg (AnyExpr) -> m AnyExpr
appAny = fmap generalize .: app

removesArg :: _ => m Bool
removesArg = do
    x  <- strVar "foldl"
    y  <- generalize <$> strVar "+"
    z  <- generalize <$> integer (0 :: Int)
    e  <- apps x [y, z]
    e' <- removeArg e 0
    args <- dumpArguments e'
    (&&) <$> isBlank (head args) <*> (not <$> isBlank (last args))

destructApp :: _ => AnyExpr -> m (AnyExpr, [AnyExpr])
destructApp app' = match app' $ \case
    App a _ -> do
        unpackedArgs <- dumpArguments app'
        target <- source a
        return (target, unpackedArgs)
    _ -> throwM NotAppException

newApplication :: _ => AnyExpr -> AnyExpr -> Int -> m AnyExpr
newApplication fun arg' pos = do
    blanks <- sequence $ replicate pos blank
    let args = generalize blanks ++ [arg']
    apps fun args

rewireApplication :: _ => AnyExpr -> AnyExpr -> Int -> m AnyExpr
rewireApplication fun arg' pos = do
    (target, oldArgs) <- destructApp fun

    let argsLength = max (pos + 1) (length oldArgs)
    blanks <- replicateM (argsLength - length oldArgs) blank
    let argsCmd = oldArgs ++ map generalize blanks
        withNewArg = argsCmd & ix pos .~ arg'

    apps target withNewArg

applyFunction :: _ => AnyExpr -> AnyExpr -> Int -> m AnyExpr
applyFunction fun arg pos = match fun $ \case
    App{} -> rewireApplication fun arg pos
    _     -> newApplication fun arg pos

data NodeMarkerMock = NodeMarkerMock
nodeMarkerKey :: HMap.TypeKey NodeMarkerMock
nodeMarkerKey = HMap.TypeKey

-- makeNodeRep :: _ => NodeMarkerMock -> Luna.Prelude.String -> AnyExpr -> m AnyExpr
-- makeNodeRep marker name node = do
--     (nameVar :: AnyExpr) <- generalize <$> strVar name
--     hmap <- readLayer @Meta nameVar
--     writeLayer @Meta (HMap.insert nodeMarkerKey marker hmap) nameVar
--     generalize <$> unify nameVar node

spec :: Spec
spec = describe "playground" $ do
    it "checks lambda" $
        graphTestCase checksLambda `shouldReturn` Right True
    it "checks app" $
        graphTestCase checksApp `shouldReturn` Right True
    it "checks blank" $
        graphTestCase checksBlank `shouldReturn` Right True
    it "dumps arguments" $
        graphTestCase checksArguments `shouldReturn` Right [Elem 3]
    it "removes arg" $
        graphTestCase removesArg `shouldReturn` Right True
