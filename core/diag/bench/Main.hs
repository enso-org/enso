{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

module Main where

import Luna.Prelude hiding (index)
import qualified Data.Graph as Graph
import           Data.Graph (Ref, Node, ELEMENT(..))

import           Type.Inference
import           Old.Luna.Pretty.GraphViz
import           Old.Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Data.Graph.Builder.Ref                          as Ref
import qualified Data.Graph.Builder.Class                        as Graph
import           Old.Luna.Syntax.Model.Layer
import           Old.Luna.Syntax.Model.Network.Builder.Node
import           Old.Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph'', NetLayers', NetCluster, runNetworkBuilderT', fmapInputs, inputstmp)
import           Old.Luna.Syntax.Model.Network.Term

import Data.Container
import Data.Container.Auto
import Data.Container.Reusable
import Data.Container.Resizable (Exponential(..))
import qualified Data.Vector                 as Boxed
import qualified Data.Vector.Unboxed         as Unboxed
import qualified Data.Container.Instances.Vector.Boxed.Mutable as BM
import           Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as M
import Control.Monad.ST

import Control.Monad.Trans.Identity

import Criterion.Main
import Control.Monad.Primitive (PrimMonad, PrimState)

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



--runBuild (g :: NetGraph'') m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers' :<: Draft Static)))
--                             $ runNetworkBuilderT' g m


--prebuild :: IO (Ref Node (NetLayers' :<: Draft Static), NetGraph'')
--prebuild = runBuild def star


type AutoVec  = Auto Exponential (Boxed.Vector Int)

autoVecEnv :: IO (AutoVec, Int)
autoVecEnv = do
    let v  = alloc 0 :: AutoVec
        el = 0 :: Int
    return (v, el)

tst :: PrimMonad m => AutoVec -> m (Auto Exponential (MVector (PrimState m) Int))
tst (v :: AutoVec) = mapM Boxed.thaw v

nativeVecEnv = do
    let i     = 100000
        v     = Boxed.fromList (replicate i 0) :: Boxed.Vector Int
    return v

nativeVecEnv' = do
    let i     = 10000000
        v     = Boxed.fromList (replicate i 0) :: Boxed.Vector Int
    return v

vecEnv = do
    let v  = alloc 0 :: Boxed.Vector Int
        el = 0 :: Int
    return (v, el)

reusableVecEnv = do
    let --v  = alloc 10000 :: Reusable Int (Boxed.Vector Int)
        i  = 100000
        v  = Reusable [0..i - 1] $ Boxed.fromList (replicate i 0) :: Reusable Int (Boxed.Vector Int)
        el = 0 :: Int
    return (v, el)


--addNodeEnv = do
--    (_,  g :: NetGraph'') <- prebuild
--    let n = fst $ nodeAdd g
--    return (g, n)


mvecEnv' :: Boxed.Vector Int
mvecEnv' = runST $ Boxed.unsafeFreeze =<< v
    where i = 100000
          v = do
              v1 <- M.replicate i (0 :: Int)
              v2 <- insertM 0 (1 :: Int) v1
              return v1

main :: IO ()
main = do
    (myv, _) <- autoVecEnv
    let myv2 = runST $ tst myv >>= addM 0 >>= mapM Boxed.unsafeFreeze
    let myv3 = runST $ tst myv >>= expandM >>= mapM Boxed.unsafeFreeze
    let myv4 = runST $ tst myv3 >>= expandM >>= mapM Boxed.unsafeFreeze
    print myv
    print myv2
    print myv3
    print $ index 0 myv4
    print "hello"
    defaultMain
        [ bgroup "native vec"
              [ --env nativeVecEnv  $ \ ~(g) -> bgroup "add elems seq"  $ take 3 $ benchNativeVecIns     g <$> doubles 800
                env nativeVecEnv' $ \ ~(g) -> bgroup "add elems bulk" $ take 6 $ benchNativeVecInsBulk g <$> doubles 200000
              ]
        , bgroup "native mutable vec"
              [ env nativeVecEnv' $ \ ~(g) -> bgroup "add elems seq"  $ take 6 $ benchNativeMVecIns  g 1 <$> doubles 200000
              ]
        , bgroup "mutable vec"
              [ env nativeVecEnv' $ \ ~(g) -> bgroup "add elems seq"  $ take 6 $ benchMVecIns        g 1 <$> doubles 200000
              ]
        , bgroup "mutable auto vec"
              [ env autoVecEnv $ \ ~(g, el) -> bgroup "add elems seq" $ take 6 $ benchMVecAdd      g el <$> doubles 200000
              ]
        --, bgroup "vec"
        --      [ env vecEnv $ \ ~(g, el) ->
        --            bgroup "add elems" $ take 5 $ benchVecAdd g el <$> doubles 800
        --      ]
        --, bgroup "reusable vec"
        --      [ env reusableVecEnv $ \ ~(g, el) ->
        --            bgroup "add elems" $ take 3 $ benchReusableVecAdd' g el <$> doubles 800
        --      ]
        --, bgroup "autovec"
        --      [ env autoVecEnv $ \ ~(g, el) ->
        --            bgroup "add elems" $ take 5 $ benchContAdd g el <$> doubles 800
        --      ]
        --, bgroup "graph"
        --      [ env addNodeEnv $ \ ~(g, el) ->
        --            bgroup "add nodes" $ take 5 $ benchNodeAdd g el <$> doubles 200
        --      ]
        ]

    --let

--unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)

benchNodeAdd :: NetGraph'' -> NetLayers' :<: Draft Static -> Int -> Benchmark
benchNodeAdd g n i = bench (show i) $ nf (flip (foldl' (flip ($))) (replicate i (nodeAddPure_ n))) g

benchContAdd :: AutoVec -> Int -> Int -> Benchmark
benchContAdd g n i = bench (show i) $ nf (flip (foldl' (flip ($))) (replicate i (add n))) g

benchVecAdd :: Boxed.Vector Int -> Int -> Int -> Benchmark
benchVecAdd g n i = bench (show i) $ nf (flip (foldl' (flip ($))) (replicate i (add n))) g

benchNativeMVecIns :: Boxed.Vector Int -> Int -> Int -> Benchmark
benchNativeMVecIns g n i = bench (show i) $ nf (\g' -> runST $ Boxed.unsafeFreeze =<< flip (foldlM (flip ($))) ((\i' v -> M.write v i' n *> pure v) <$> [0..i]) =<< Boxed.thaw g :: Boxed.Vector Int) g

benchMVecIns :: Boxed.Vector Int -> Int -> Int -> Benchmark
benchMVecIns g n i = bench (show i) $ nf (\g' -> runST $ Boxed.unsafeFreeze =<< flip (foldlM (flip ($))) (flip insertM n <$> [0..i]) =<< Boxed.thaw g :: Boxed.Vector Int) g

benchMVecAdd :: AutoVec -> Int -> Int -> Benchmark
benchMVecAdd g n i = bench (show i) $ nf (\g' -> runST $ mapM Boxed.unsafeFreeze =<< flip (foldlM (flip ($))) (replicate i (addM n)) =<< mapM Boxed.thaw g :: AutoVec) g

benchReusableVecAdd :: Reusable Int (Boxed.Vector Int) -> Int -> Int -> Benchmark
benchReusableVecAdd g n i = bench (show i) $ nf (flip (foldl' (flip ($))) (replicate i (add n))) g

benchReusableVecAdd' :: Reusable Int (Boxed.Vector Int) -> Int -> Int -> Benchmark
benchReusableVecAdd' g n i = bench (show i) $ nf (head ∘ toList ∘ flip (foldl' (flip ($))) (replicate i (add n))) g

benchNativeVecIns :: Boxed.Vector Int -> Int -> Benchmark
benchNativeVecIns g i = bench (show i) $ nf (flip (foldl' (flip ($))) (take i (flip Boxed.unsafeUpd ∘ (:[]) <$> zip [0..] (repeat 0)))) g

benchNativeVecInsBulk :: Boxed.Vector Int -> Int -> Benchmark
benchNativeVecInsBulk g i = bench (show i) $ nf (flip Boxed.unsafeUpd $ take i $ zip [0..] (repeat 0)) g

benchNativeVecAdd :: Boxed.Vector Int -> Int -> Int -> Benchmark
benchNativeVecAdd g n i = bench (show i) $ nf (flip (foldl' (flip ($))) (replicate i (flip Boxed.snoc n))) g

doubles :: Int -> [Int]
doubles i = i : doubles (2 * i)


nodeAddPure_ :: NetLayers' :<: Draft Static -> NetGraph'' -> NetGraph''
nodeAddPure_ = snd ∘∘ nodeAddPure

nodeAddPure :: NetLayers' :<: Draft Static -> NetGraph'' -> (Ref Node (NetLayers' :<: Draft Static), NetGraph'')
nodeAddPure = Graph.add
--foo = force (undefined :: '[] Draft Static)

-----------------------
-- === Showcase === ---
-----------------------

--showcase :: IO ()
--showcase = do
--    (_,  g :: NetGraph'') <- prebuild
--    (_, g') <- foo g
--    renderAndOpen [ ("g", "g", g')
--                  ]

--nodeAdd_ :: NetGraph'' -> NetGraph''
--nodeAdd_ = snd ∘ nodeAdd

--nodeAdd :: NetGraph'' -> (NetLayers' :<: Draft Static, NetGraph'')
--nodeAdd g = runIdentity $ runNetworkBuilderT' g $ do
--    s1   <- int 8
--    s1_v <- read s1
--    return s1_v



-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- FIXME W Luna.Syntax.AST.Term.Class

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
