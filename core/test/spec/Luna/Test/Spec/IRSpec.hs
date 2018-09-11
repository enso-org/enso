{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Luna.Test.Spec.IRSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Control.Monad.Exception               as Exception
import qualified Data.Generics.Traversable             as GTraversable
import qualified Data.Graph.Component.Edge.Class       as Edge
import qualified Data.Graph.Data.Component.Class       as Component
import qualified Data.Graph.Data.Component.Set         as PtrSet
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.Graph.Fold.Class                 as Fold
import qualified Data.Graph.Fold.Partition             as Partition
import qualified Data.Graph.Fold.SubComponents         as Traversal
import qualified Data.Graph.Fold.SubTree               as Traversal
import qualified Data.Graph.Fold.SubTree               as SubTree
import qualified Data.Graph.Store                      as Store
import qualified Data.Graph.Store.Size.Discovery       as Size
import qualified Data.List                             as List
import qualified Data.Mutable.Class                    as Mutable
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Data.Mutable.Storable.SmallSet        as SmallSet
import qualified Data.Set                              as StdSet
import qualified Data.Vector.Storable.Foreign          as Vector
import qualified Foreign.Marshal.Alloc                 as Mem
import qualified Foreign.Storable.Utils                as Storable
import qualified Luna.IR                               as IR
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Basic                       as Pass
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified System.Random                         as Random

import qualified Data.Graph.Store.Buffer  as Buffer
import qualified Luna.IR.Term.Ast.Invalid as InvalidIR

import Data.Graph.Data.Graph.Class           (Graph)
import Data.Mutable.Storable.SmallAutoVector (SmallVector, UnmanagedSmallVector)
import Luna.Pass                             (Pass)
import Luna.Pass.Basic                       (Compilation)
import Test.Hspec                            (Spec, describe, it)



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


-- === API === --

type OnDemandPass stage pass m =
    ( Typeable pass
    , Pass.Compile stage pass m
    , MonadIO m
    , Exception.MonadException Scheduler.Error m
    )

runPass :: ∀ stage pass m. OnDemandPass stage pass m => Pass stage pass () -> m ()
runPass = runPasses . pure

runPasses :: ∀ stage pass m. OnDemandPass stage pass m => [Pass stage pass ()] -> m ()
runPasses passes = Scheduler.evalT $ do
    Scheduler.registerAttr     @IntAttr
    Scheduler.enableAttrByType @IntAttr
    for_ passes $ \pass -> do
        Scheduler.registerPassFromFunction__ pass
        Scheduler.runPassByType @pass

run2Passes :: ∀ stage pass m. OnDemandPass stage pass m => Pass stage pass () -> Pass stage pass () -> m ()
run2Passes p1 p2 = runPasses [p1,p2]

runPass' :: Pass Pass.Compilation TestPass () -> IO ()
runPass' p = Graph.encodeAndEval @Pass.Compilation $ runPass p

run2Passes' :: Pass Pass.Compilation TestPass () -> Pass Pass.Compilation TestPass () -> IO ()
run2Passes' p1 p2 = Graph.encodeAndEval @Pass.Compilation $ runPasses [p1,p2]



-------------------
-- === Tests === --
-------------------

nameSpec :: Spec
nameSpec = describe "names" $ do
    it "encoding" $ runPass' $ do
        v        <- IR.var "a"
        IR.Var n <- IR.modelView v
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
        IR.Unify l r <- IR.modelView u1
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
        IR.Unify l r <- IR.modelView u1
        v1_users     <- Mutable.toList =<< Layer.read @IR.Users v1
        v2_users     <- Mutable.toList =<< Layer.read @IR.Users v2
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

-- partitionSpec :: Spec
-- partitionSpec = describe "Component children partition" $ do
--     it "should not be wrong" $ runPass' $ do
--         v <- IR.var "a"
--         ps <- Partition.partition v
--         st <- SubTree.subTree' v
--         1 `shouldBe` 1


testVec :: Spec
testVec = describe "test" $ it "vec" $ do
    print ""
    (print "--- vec ---" :: IO ())
    (a :: UnmanagedSmallVector 0 Int) <- Mutable.new
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    Mutable.pushBack a 8
    print "--"
    print a
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    Mutable.pushBack a 7
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    print =<< Mutable.unsafeRead a 1
    Mutable.pushBack a 6
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    print =<< Mutable.unsafeRead a 1
    print =<< Mutable.unsafeRead a 2
    Mutable.pushBack a 5
    Mutable.pushBack a 4
    Mutable.pushBack a 3
    Mutable.pushBack a 2
    Mutable.pushBack a 1
    Mutable.pushBack a 10
    print "--"
    print =<< SmallVector.elemsPtr a
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.unsafeRead a 0
    print =<< Mutable.unsafeRead a 1
    print =<< Mutable.unsafeRead a 2
    print =<< Mutable.unsafeRead a 3
    print =<< Mutable.unsafeRead a 4
    print =<< Mutable.unsafeRead a 5
    print =<< Mutable.unsafeRead a 6
    print =<< Mutable.unsafeRead a 7
    print =<< Mutable.unsafeRead a 8

    True `shouldBe` False


testSet :: Spec
testSet = describe "test" $ it "set" $ do
    print ""
    (print "--- set ---" :: IO ())
    (a :: SmallSet.SmallSet 0 Int) <- Mutable.new
    print =<< Mutable.size a
    Mutable.insert a 8
    Mutable.insert a 8
    Mutable.insert a 8
    Mutable.remove a 9
    Mutable.insert a 8
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.toList a
    Mutable.insert a 7
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.toList a
    Mutable.insert a 6
    Mutable.insert a 6
    Mutable.insert a 6
    Mutable.insert a 6
    Mutable.insert a 6
    Mutable.remove a 7
    Mutable.insert a 8
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.toList a
    Mutable.insert a 5
    Mutable.insert a 4
    Mutable.insert a 3
    Mutable.insert a 2
    Mutable.insert a 1
    Mutable.insert a 10
    print "--"
    print =<< Mutable.size a
    print =<< Mutable.capacity a
    print =<< Mutable.toList a

    print "============="

    flip mapM [0 .. 10000] $ \i -> do
        s <- Random.getStdRandom (Random.randomR(1,100))
        if i `mod` 2 == 1
            then Mutable.insert a s
            else Mutable.remove a s

    lst <- Mutable.toList a
    print lst
    print (lst == List.nub lst)


    True `shouldBe` False

test :: Spec
test = describe "test" $ it "test" $ runPass' $ do

    print "=============="
    print "=============="
    print "=============="

    v  <- IR.var "s"

    vn <- Mutable.fromList ["foo", "bar", "baz"]
    u <- IR.update v vn v
    IR.Update vu1 _ vu2 <- IR.modelView u




    -- users <- Layer.read @IR.Users v
    putStrLn "\n=== elements ==="
    vtpl    <- Layer.read @IR.Type v
    vtp     <- Layer.read @IR.Source vtpl
    vttpl   <- Layer.read @IR.Type vtp

    putStrLn $ ": v     = " <> show v
    putStrLn $ ": u     = " <> show u
    putStrLn $ ": vu1   = " <> show vu1
    putStrLn $ ": vu2   = " <> show vu2
    putStrLn $ ": vtpl  = " <> show vtpl
    putStrLn $ ": vtp   = " <> show vtp
    putStrLn $ ": vttpl = " <> show vttpl


    -- print $ "tp: " <> show tp
    -- print =<< PtrSet.toList users
    -- print =<< PtrSet.size   users
    -- print "***"

    -- print =<< Size.discoverDynamic v
    -- print vn

    -- print "--------------------------"

    buffer <- Store.serialize u
    putStrLn "\n---------------------\n"

    u' <- Store.deserialize @IR.Terms buffer
    m  <- Layer.read @IR.Model u'
    print $ IR.showTag m
    print u'

    -- case m of
    --     IR.UniTermVar (IR.Var n) -> Layer.write @IR.Model c (IR.UniTermVar (IR.Var "X"))
    --     _                        -> print "nope :("

    putStrLn $ ": v      = " <> show v
    putStrLn $ ": u      = " <> show u
    putStrLn $ ": vu1    = " <> show vu1
    putStrLn $ ": vu2    = " <> show vu2
    putStrLn $ ": vtpl   = " <> show vtpl
    putStrLn $ ": vtp    = " <> show vtp
    putStrLn $ ": vttpl  = " <> show vttpl
    putStrLn ""

    m <- Layer.read @IR.Model u'
    case m of
        IR.UniTermUpdate (IR.Update vu1' n vu2') -> do

            v' <- Layer.read @IR.Source vu1'

            putStrLn $ ": v'     = " <> show v'
            putStrLn $ ": vu1'   = " <> show vu1'
            putStrLn $ ": vu2'   = " <> show vu2'

            vm' <- Layer.read @IR.Model v'
            print $ IR.showTag vm'
        _ -> do
            fail $ "Wrong deserialization: " <> convert (IR.showTag m)

    -- vm <- Layer.read @IR.Model v
    -- case vm of
    --     IR.UniTermVar (IR.Var n) -> print ("!!!", n)
    --     _                        -> print "nope :("

    -- True `shouldBe` True

delTest :: Spec
delTest = describe "delete" $ it "test" $ runPass' $ do
    print "create"
    v1 <- IR.var "v1"
    v2 <- IR.var "v2"
    c  <- IR.cons "A" []
    print "delete"
    IR.destruct1 c
    print "done"


subIRTest2 :: Spec
subIRTest2 =  describe "subir" $ it "test2" $ runPass' $ do
    v1  <- IR.blank
    v2  <- IR.blank
    v3  <- IR.blank
    l1  <- IR.lam v1 v2
    l2  <- IR.lam v3 l1
    g   <- IR.grouped l2

    v1_tpl    <- Layer.read @IR.Type v1
    v1_tp     <- Layer.read @IR.Source v1_tpl
    v1_ttpl   <- Layer.read @IR.Type v1_tp
    v1_users  <- Mutable.toList =<< Layer.read @IR.Users v1
    v1_tp_users  <- Mutable.toList =<< Layer.read @IR.Users v1_tp

    v2_tpl    <- Layer.read @IR.Type v2
    v2_tp     <- Layer.read @IR.Source v2_tpl
    v2_ttpl   <- Layer.read @IR.Type v2_tp
    v2_users  <- Mutable.toList =<< Layer.read @IR.Users v2
    v2_tp_users  <- Mutable.toList =<< Layer.read @IR.Users v2_tp

    v3_tpl    <- Layer.read @IR.Type v3
    v3_tp     <- Layer.read @IR.Source v3_tpl
    v3_ttpl   <- Layer.read @IR.Type v3_tp
    v3_users  <- Mutable.toList =<< Layer.read @IR.Users v3
    v3_tp_users  <- Mutable.toList =<< Layer.read @IR.Users v3_tp

    IR.Lam l1_A l1_B <- IR.modelView l1
    l1_tpl    <- Layer.read @IR.Type l1
    l1_tp     <- Layer.read @IR.Source l1_tpl
    l1_ttpl   <- Layer.read @IR.Type l1_tp
    l1_users  <- Mutable.toList =<< Layer.read @IR.Users l1
    l1_tp_users  <- Mutable.toList =<< Layer.read @IR.Users l1_tp

    IR.Lam l2_A l2_B <- IR.modelView l2
    l2_tpl    <- Layer.read @IR.Type l2
    l2_tp     <- Layer.read @IR.Source l2_tpl
    l2_ttpl   <- Layer.read @IR.Type l2_tp
    l2_users  <- Mutable.toList =<< Layer.read @IR.Users l2
    l2_tp_users  <- Mutable.toList =<< Layer.read @IR.Users l2_tp

    IR.Grouped g_A <- IR.modelView g
    g_tpl    <- Layer.read @IR.Type g
    g_tp     <- Layer.read @IR.Source g_tpl
    g_ttpl   <- Layer.read @IR.Type g_tp
    g_users  <- Mutable.toList =<< Layer.read @IR.Users g
    g_tp_users  <- Mutable.toList =<< Layer.read @IR.Users g_tp

    -- putStrLn $ "v1           = " <> show v1
    -- putStrLn $ "v1_tpl       = " <> show v1_tpl
    -- putStrLn $ "v1_tp        = " <> show v1_tp
    -- putStrLn $ "v1_ttpl      = " <> show v1_ttpl
    -- putStrLn $ "v1_users     = " <> show v1_users
    -- putStrLn $ "v1_tp_users  = " <> show v1_tp_users
    -- putStrLn ""
    -- putStrLn $ "v2           = " <> show v2
    -- putStrLn $ "v2_tpl       = " <> show v2_tpl
    -- putStrLn $ "v2_tp        = " <> show v2_tp
    -- putStrLn $ "v2_ttpl      = " <> show v2_ttpl
    -- putStrLn $ "v2_users     = " <> show v2_users
    -- putStrLn $ "v2_tp_users  = " <> show v2_tp_users
    -- putStrLn ""
    -- putStrLn $ "v3           = " <> show v3
    -- putStrLn $ "v3_tpl       = " <> show v3_tpl
    -- putStrLn $ "v3_tp        = " <> show v3_tp
    -- putStrLn $ "v3_ttpl      = " <> show v3_ttpl
    -- putStrLn $ "v3_users     = " <> show v3_users
    -- putStrLn $ "v3_tp_users  = " <> show v3_tp_users
    -- putStrLn ""
    -- putStrLn $ "l1          = " <> show l1
    -- putStrLn $ "l1_A        = " <> show l1_A
    -- putStrLn $ "l1_B        = " <> show l1_B
    -- putStrLn $ "l1_tpl      = " <> show l1_tpl
    -- putStrLn $ "l1_tp       = " <> show l1_tp
    -- putStrLn $ "l1_ttpl     = " <> show l1_ttpl
    -- putStrLn $ "l1_users    = " <> show l1_users
    -- putStrLn $ "l1_tp_users = " <> show l1_tp_users
    -- putStrLn ""
    -- putStrLn $ "l2          = " <> show l2
    -- putStrLn $ "l2_A        = " <> show l2_A
    -- putStrLn $ "l2_B        = " <> show l2_B
    -- putStrLn $ "l2_tpl      = " <> show l2_tpl
    -- putStrLn $ "l2_tp       = " <> show l2_tp
    -- putStrLn $ "l2_ttpl     = " <> show l2_ttpl
    -- putStrLn $ "l2_users    = " <> show l2_users
    -- putStrLn $ "l2_tp_users = " <> show l2_tp_users
    -- putStrLn ""
    -- putStrLn $ "g          = " <> show g
    -- putStrLn $ "g_A        = " <> show g_A
    -- putStrLn $ "g_tpl      = " <> show g_tpl
    -- putStrLn $ "g_tp       = " <> show g_tp
    -- putStrLn $ "g_ttpl     = " <> show g_ttpl
    -- putStrLn $ "g_users    = " <> show g_users
    -- putStrLn $ "g_tp_users = " <> show g_tp_users

    -- print "!!!"
    -- print . IR.showTag =<< Layer.read @IR.Model v
    -- print =<< Layer.read @IR.Users v_tp


    Store.serialize (Layout.relayout g :: IR.SomeTerm)
    return ()

    True `shouldBe` True

spec :: Spec
spec = do

    -- testVec
    -- testSet
    -- test
    nameSpec
    irCreationSpec
    attribsSpec
    irDestructSpec
    irDiscoverySpec
    delTest
    subIRTest2
    -- partitionSpec




-- TODO: REMOVE REMOVE REMOVE
-- instance Applicative m => Buffer.CopyInitializerP1 m IR.UniTerm



