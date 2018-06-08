{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Luna.Test.Spec.IRSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Control.Monad.Exception         as Exception
import qualified Data.Graph.Component.Edge.Class as Edge
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Graph.Fold.Class           as Fold
import qualified Data.Graph.Fold.Partition       as Partition
import qualified Data.Graph.Fold.SubComponents   as Traversal
import qualified Data.Graph.Fold.SubTree         as Traversal
import qualified Data.Graph.Fold.SubTree         as SubTree
import qualified Data.Set                        as StdSet
import qualified Data.Set.Mutable.Class          as Set
import qualified Luna.IR                         as IR
import qualified Luna.IR.Layer                   as Layer
import qualified Luna.Pass                       as Pass
import qualified Luna.Pass.Attr                  as Attr
import qualified Luna.Pass.Basic                 as Pass
import qualified Luna.Pass.Scheduler             as Scheduler

import Data.Graph.Data.Graph.Class (Graph)
import Luna.Pass                   (Pass)
import Luna.Pass.Basic             (Compilation)
import Test.Hspec                  (Spec, describe, it)



-----------------------
-- === Test pass === --
-----------------------

-- === Definition === --

newtype IntAttr = IntAttr Int deriving (Show, Eq, Num)
type instance Attr.Type IntAttr = Attr.Atomic
instance Default IntAttr where
    def = IntAttr 0

data TestPass
type instance Pass.Spec TestPass t = TestPassSpec t
type family   TestPassSpec  t where
    TestPassSpec (Pass.In  Pass.Attrs) = Pass.List '[IntAttr]
    TestPassSpec (Pass.Out Pass.Attrs) = Pass.List '[IntAttr]
    TestPassSpec t                     = Pass.BasicPassSpec t


-- === API === --

type OnDemandPass pass m =
    ( Typeable pass
    , Pass.Compile pass m
    , MonadIO m
    , Exception.MonadException Scheduler.Error m
    )

runPass :: ∀ pass m. OnDemandPass pass m => Pass pass () -> m ()
runPass = runPasses . pure

runPasses :: ∀ pass m. OnDemandPass pass m => [Pass pass ()] -> m ()
runPasses passes = Scheduler.evalT $ do
    Scheduler.registerAttr     @IntAttr
    Scheduler.enableAttrByType @IntAttr
    for_ passes $ \pass -> do
        Scheduler.registerPassFromFunction__ pass
        Scheduler.runPassByType @pass

run2Passes :: ∀ pass m. OnDemandPass pass m => Pass pass () -> Pass pass () -> m ()
run2Passes p1 p2 = runPasses [p1,p2]

runPass' :: Pass TestPass () -> IO ()
runPass' p = Graph.encodeAndEval @Pass.Compilation $ runPass p

run2Passes' :: Pass TestPass () -> Pass TestPass () -> IO ()
run2Passes' p1 p2 = Graph.encodeAndEval @Pass.Compilation $ runPasses [p1,p2]



-------------------
-- === Tests === --
-------------------

nameSpec :: Spec
nameSpec = describe "names" $ do
    it "encoding" $ runPass' $ do
        v        <- IR.var "a"
        IR.Var n <- IR.model v
        n `shouldBe`    "a"
        n `shouldNotBe` "b"

irCreationSpec :: Spec
irCreationSpec = describe "ir creation" $ do
    it "single term" $ runPass' $ do
        v   <- IR.var "a"
        m   <- Layer.read @IR.Model v
        tag <- IR.showTag <$> Layer.read @IR.Model v
        m   `shouldBe` IR.UniTermVar (IR.Var "a")
        tag `shouldBe` "VAR"

    it "complex term" $ runPass' $ do
        v1           <- IR.var "a"
        v2           <- IR.var "b"
        u1           <- IR.unify v1 v2
        IR.Unify l r <- IR.model u1
        lsrc         <- Layer.read @IR.Source l
        rsrc         <- Layer.read @IR.Source r
        ltgt         <- Layer.read @IR.Target l
        rtgt         <- Layer.read @IR.Target r
        lnks         <- IR.inputs u1

        tp  <- Layer.read @IR.Type u1
        mod <- Layer.read @IR.Model u1

        lsrc `shouldBe` v1
        ltgt `shouldBe` u1
        rsrc `shouldBe` v2
        rtgt `shouldBe` u1

        u1_subComps <- StdSet.fromList . toList
                   <$> Traversal.subComponents @IR.Links u1
        let u1_subComps' = StdSet.fromList
                         [ Layout.relayout tp
                         , Layout.relayout l
                         , Layout.relayout r
                         ]

        u1_subComps `shouldBe` u1_subComps'

    it "users layer" $ runPass' $ do
        v1           <- IR.var "a"
        v2           <- IR.var "b"
        u1           <- IR.unify v1 v2
        IR.Unify l r <- IR.model u1
        v1_users     <- Set.toList =<< Layer.read @IR.Users v1
        v2_users     <- Set.toList =<< Layer.read @IR.Users v2
        v1_users `shouldBe` [Layout.relayout l]
        v2_users `shouldBe` [Layout.relayout r]

attribsSpec :: Spec
attribsSpec = describe "attributes" $ do
    it "passing between passes" $ run2Passes'
        (Attr.put $ IntAttr 9)
        (Attr.get >>= (`shouldBe` (IntAttr 9)))

irDestructSpec :: Spec
irDestructSpec = describe "ir dispose" $ do
    it "simple" $ runPass' $ do
        v <- IR.var "a"
        IR.destruct v

irDiscoverySpec :: Spec
irDiscoverySpec = describe "traversal" $ do
    it "discovery" $ runPass' $ do
        v     <- IR.var "a"
        terms <- Traversal.subTree' v

        tpl  <- Layer.read @IR.Type v
        tp   <- Layer.read @IR.Source tpl
        ttpl <- Layer.read @IR.Type tp

        let termSet  = StdSet.fromList $ toList terms
            allNodes = StdSet.fromList
                     $ [ Layout.relayout v
                       , Layout.relayout tpl
                       , Layout.relayout tp
                       , Layout.relayout ttpl
                       ]
        termSet `shouldBe` allNodes

partitionSpec :: Spec
partitionSpec = describe "Component children partition" $ do
    it "should not be wrong" $ runPass' $ do
        v <- IR.var "a"
        ps <- Partition.partition v
        st <- SubTree.subTree' v
        1 `shouldBe` 1

spec :: Spec
spec = do
    nameSpec
    irCreationSpec
    attribsSpec
    irDestructSpec
    irDiscoverySpec
    partitionSpec
