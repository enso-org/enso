{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoOverloadedStrings  #-}


module Main where


import qualified Data.ByteString.Lazy.Char8 as ByteString

import Luna.Prelude hiding (String, typeRep)
import Data.Aeson (encode)

import           Luna.IR
import qualified Luna.IR.Repr.Vis as Vis
import           Luna.IR.Repr.Vis (MonadVis, snapshot)
import           Luna.Pass        (Pass, Inputs, Outputs, Preserves)
import qualified Luna.Pass        as Pass


import Luna.IR.Expr.Term.Class


data MyData = MyData Int deriving (Show)


data                    SimpleAA
type instance Inputs    SimpleAA = '[Attr MyData, ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID, Type] <> ExprLinkLayers '[Model, UID]
type instance Outputs   SimpleAA = '[Attr MyData, ExprNet, ExprLinkNet] <> ExprLayers '[Model, UID, Type] <> ExprLinkLayers '[Model, UID]
type instance Preserves SimpleAA = '[]

pass1 :: (MonadFix m, MonadIO m, IRMonad m, MonadVis m) => Pass SimpleAA m
pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m) => m (Either Pass.Err ())
test_pass1 = runIRT $ do
    runRegs

    attachLayer (typeRep @Model) (typeRep @EXPR)
    attachLayer (typeRep @Succs) (typeRep @EXPR)
    attachLayer (typeRep @Type)  (typeRep @EXPR)
    attachLayer (typeRep @UID)   (typeRep @EXPR)

    attachLayer (typeRep @Model) (typeRep @(LINK' EXPR))
    attachLayer (typeRep @UID)   (typeRep @(LINK' EXPR))

    setAttr $ MyData 7

    Pass.eval pass1



gen_pass1 :: ( MonadIO m, IRMonad m, MonadVis m
             , Accessibles m '[ExprLayer Model, ExprLinkLayer Model, ExprLayer Type, ExprLinkLayer UID, ExprLayer UID, ExprNet, ExprLinkNet, Attr MyData]
             ) => m ()
gen_pass1 = layouted @Ent $ do
    -- (s :: Expr (ENT Star   ()     Star)) <- star
    (aName :: Expr (ENT String String Star)) <- string "a"
    (a :: Expr Int Star)) <- var aName
    b <- var "b"

    -- (u :: Expr (ENT _ _ _)) <- unify a b
    -- -- (f :: Expr (ENT Star Star Star)) <- acc "f" u
    --
    --
    --
    -- -- snapshot "s3"
    -- d <- readLayer @Type u
    -- print d
    --
    --
    -- md <- readAttr @MyData
    -- print md
    --
    -- ts <- exprs
    -- print ts
    --
    -- match s $ \case
    --     Unify l r -> print "ppp"
    --     Star      -> match s $ \case
    --         Unify l r -> print "hola"
    --         Star      -> print "hellox"
    --
    -- print "---"
    --
    --
    -- match a $ \ (Var l) -> do
    --     n <- source l
    --     match n $ \case
    --         String s -> print s
    --
    -- v <- var "ala"
    -- n <- strName v
    -- (s3 :: Expr (ENT Draft String Draft)) <- generalize <$> star
    -- (v3 :: Expr (ENT Draft String Draft)) <- generalize <$> var "lel"
    -- match v3 $ \(Var l) -> do
    --     print "IT'S VAR"
    --     n' <- source l
    --     n  <- match n' $ \case
    --         String s -> return s
    --     print n
    --
    -- print n

    return ()




main :: IO ()
main = do
    (p, vis) <- Vis.newRunDiffT test_pass1
    case p of
        Left e -> do
            print "* INTERNAL ERROR *"
            print e
        Right _ -> do
            let cfg = ByteString.unpack $ encode $ vis
            -- putStrLn cfg
            {-liftIO $ openBrowser ("http://localhost:8200?cfg=" <> cfg)-}
            return ()
    print p
    return ()
