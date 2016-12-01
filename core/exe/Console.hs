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
    (s1 :: Expr (ENT Star   ()     Star)) <- star
    (s2 :: Expr (ENT Star   ()     Star)) <- star
    snapshot "s1"
    (n  :: Expr (ENT String ()     Star)) <- string "hello"
    (v  :: Expr (ENT Var    String Star)) <- var n
    (v2 :: Expr (ENT Var    String Star)) <- var "foo"
    let (ng :: Expr (ENT Draft Draft Draft)) = generalize n
    snapshot "s2"
    u1 <- unify s1 s2
    snapshot "s3"
    print "hello"
    d <- readLayer @Type u1
    print d
    md <- readAttr @MyData
    print md
    ts <- exprs
    print ts

    match s1 $ \case
        Unify l r -> print "ppp"
        Star      -> match s1 $ \case
            Unify l r -> print "hola"
            Star      -> print "hellox"

    print "---"


    match v $ \ (Var l) -> do
        n <- source l
        match n $ \case
            String s -> print s

    v <- var "ala"
    n <- strName v
    print n

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
            -- liftIO $ openBrowser ("http://localhost:8200?cfg=" <> cfg)
            return ()
    print p
    return ()
