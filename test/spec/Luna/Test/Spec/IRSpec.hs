{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Spec.IRSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

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
import qualified OCI.IR.Layout                       as Layout

import Luna.Pass  (Pass)
import Test.Hspec (Expectation, Spec, describe, it)


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


-- TODO: remove vvv
type instance Layout.Default IR.Terms = ()

-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "IR creation" $ do
        it "single term" $ runPass' $ do
            v <- IR.var 7
            m <- Layer.read @IR.Model v
            m `shouldBe` (IR.UniTermVar $ IR.Var 7)

        it "connections" $ runPass' $ do
            v1 <- IR.var 1
            v2 <- IR.var 2
            v3 <- IR.var 3
            v4 <- IR.var 4
            l1 <- Link.new v1 v2
            l2 <- Link.new v3 v2
            l1_src <- Layer.read @IR.Source l1
            l1_tgt <- Layer.read @IR.Target l1
            l2_src <- Layer.read @IR.Source l2
            l2_tgt <- Layer.read @IR.Target l2
            l1_src `shouldSatisfy` (== v1)
            l1_tgt `shouldSatisfy` (== v2)
            l2_src `shouldSatisfy` (== v3)
            l2_tgt `shouldSatisfy` (== v2)
            print "!!!1"
            let (u1c :: IR.ConsUnify ()) =
                  (IR.Unify (Layout.unsafeRelayout l1)
                            (Layout.unsafeRelayout l2)
                  )
                u1cc = IR.UniTermUnify u1c
            ptr <- liftIO $ Mem.mallocBytes 300
            liftIO $ Storable.poke ptr u1cc
            out <- liftIO $ Storable.peek ptr
            print ">>>"
            print (out == u1cc)
            print u1cc
            print out
            print "!!!2"
            (u1 :: IR.Term IR.Unify) <- Term.uncheckedNew @IR.Unify u1c
            print "!!!3"
            Layer.read @IR.Model u1 >>= \case
                IR.UniTermUnify (IR.Unify l r) -> do
                    print "---"
                    print l
                    print l1
                    print r
                    print l2
                    -- print "!!!4"
                    -- lsrc <- Layer.read @IR.Source l
                    -- ltgt <- Layer.read @IR.Target l
                    -- rsrc <- Layer.read @IR.Source r
                    -- rtgt <- Layer.read @IR.Target r
                    -- print "!!!"
                    -- print lsrc
                    -- print ltgt
                    -- print rsrc
                    -- print rtgt
                    -- putStrLn ""
                    -- print v1
                    -- print v2
                    -- print u1
                    -- print "---"
                    -- print rsrc
                    -- print rtgt
                    False `shouldBe` True
                _ -> 'a' `shouldBe` 'b'

            print "done"


        -- it "complex term" $ runPass' $ do
        --     v1 <- IR.var 7
        --     v2 <- IR.var 9
        --     u1 <- IR.unify v1 v1
        --     Layer.read @IR.Model u1 >>= \case
        --         IR.UniTermUnify (IR.Unify l r) -> do
        --             lsrc <- Layer.read @IR.Source l
        --             ltgt <- Layer.read @IR.Target l
        --             rsrc <- Layer.read @IR.Source r
        --             rtgt <- Layer.read @IR.Target r
        --             print "!!!"
        --             print lsrc
        --             print ltgt
        --             print rsrc
        --             print rtgt
        --             putStrLn ""
        --             print v1
        --             print v2
        --             print u1
        --             print "---"
        --             print rsrc
        --             print rtgt
        --             -- lsrc `shouldSatisfy` (== v1)
        --             -- ltgt `shouldSatisfy` (== u1)
        --             rsrc `shouldSatisfy` (== v2)
        --             -- rtgt `shouldSatisfy` (== u1)
        --         _ -> fail "Wrong encoding"

    describe "Attributes" $ do
        it "Passing between passes" $ run2Passes' (Attr.put $ IntAttr 9) $ do
            Attr.get >>= (`shouldBe` (IntAttr 9))
    pure ()
