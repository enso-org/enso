{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Spec.IRSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Data.Set.Mutable.Class              as Set
import qualified Foreign.Marshal.Alloc               as Mem
import qualified Foreign.Storable                    as Storable
import qualified Luna.IR                             as IR
import qualified Luna.IR.Component.Link              as Link
import qualified Luna.IR.Component.Term.Construction as Term
import qualified Luna.IR.Layer                       as Layer
import qualified Luna.Pass                           as Pass
import qualified Luna.Pass.Attr                      as Attr
import qualified Luna.Pass.Scheduler                 as Scheduler
import qualified Luna.Runner                         as Runner
import qualified OCI.Data.Name                       as Name
import qualified OCI.IR.Component                    as Component
import qualified OCI.IR.Layout                       as Layout

import Luna.IR.Component.Link (type (*-*), Link)
import Luna.Pass              (Pass)
import Test.Hspec             (Expectation, Spec, describe, it)


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
    TestPassSpec (Pass.In  Pass.Attrs) = '[IntAttr]
    TestPassSpec (Pass.Out Pass.Attrs) = '[IntAttr]
    TestPassSpec t                     = Pass.BasicPassSpec t

Pass.cache_phase1 ''TestPass
Pass.cache_phase2 ''TestPass


-- === API === --

type OnDemandPass pass = (Typeable pass, Pass.Compile pass IO)

runPass :: ∀ pass. OnDemandPass pass => Pass pass () -> IO ()
runPass = runPasses . pure

runPasses :: ∀ pass. OnDemandPass pass => [Pass pass ()] -> IO ()
runPasses passes = Runner.runManual $ do
    Scheduler.registerAttr     @IntAttr
    Scheduler.enableAttrByType @IntAttr
    for_ passes $ \pass -> do
        Scheduler.registerPassFromFunction__ pass
        Scheduler.runPassByType @pass

run2Passes :: ∀ pass. OnDemandPass pass => Pass pass () -> Pass pass () -> IO ()
run2Passes p1 p2 = runPasses [p1,p2]

runPass' :: Pass TestPass () -> IO ()
runPass' = runPass

run2Passes' :: Pass TestPass () -> Pass TestPass () -> IO ()
run2Passes' p1 p2 = runPasses [p1,p2]



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
        v <- IR.var "a"
        m <- Layer.read @IR.Model v
        m `shouldBe` IR.UniTermVar (IR.Var "a")

    it "complex term" $ runPass' $ do
        v1           <- IR.var "a"
        v2           <- IR.var "b"
        u1           <- IR.unify v1 v2
        IR.Unify l r <- IR.model u1
        lsrc         <- Layer.read @IR.Source l
        rsrc         <- Layer.read @IR.Source r
        ltgt         <- Layer.read @IR.Target l
        rtgt         <- Layer.read @IR.Target r
        lsrc `shouldBe` v1
        ltgt `shouldBe` u1
        rsrc `shouldBe` v2
        rtgt `shouldBe` u1

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

irDisposeSpec :: Spec
irDisposeSpec = describe "ir dispose" $ do
    it "simple" $ runPass' $ do
        v <- IR.var "a"
        IR.dispose v


spec :: Spec
spec = do
    nameSpec
    irCreationSpec
    attribsSpec
    irDisposeSpec
