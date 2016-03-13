{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

module Main where

import Prelude.Luna
import           Data.Graph
import           Data.Graph.Builders
--import           Prologue                                        hiding (Version, cons, read, ( # ), Num, Cons)

--import           Text.Printf                                     (printf)
--import qualified Control.Monad.Writer                            as Writer
import           Control.Monad.Event
--import           Data.Attr                                       (attr)
--import           Data.Construction
--import           Data.Container                                  (elems, index_)
--import           Data.Container                                  hiding (impossible)
import           Data.Graph.Builder
import           Data.Graph.Query                                hiding (Graph)
import qualified Data.Graph.Query                                as Sort
import           Data.Index                                      (idx)
--import           Data.Layer.Cover
--import qualified Data.Map                                        as Map
--import           Data.Prop
import           Data.Record                                     hiding (Layout, cons)
import           Data.Version.Semantic
--import           Development.Placeholders
import           Type.Inference

import qualified Luna.Compilation.Env.Class                      as Env
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Inference.Unification     (UnificationPass (..))
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import           Luna.Compilation.Pass.Utils.SubtreeWalk         (subtreeWalk)
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import qualified Luna.Evaluation.Runtime                         as Runtime
import qualified Luna.Evaluation.Model                           as EvalModel
import qualified Luna.Library.Standard                           as StdLib
import qualified Luna.Library.Symbol.Class                       as Symbol
import           Luna.Syntax.AST.Term                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target, Input)
import qualified Luna.Syntax.AST.Term                            as Term
import qualified Luna.Syntax.AST.Term.Lit                        as Lit
import           Data.Graph.Builder.Ref                          as Ref
import qualified Data.Graph.Builder.Class                        as Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph'', NetLayers', NetCluster, runNetworkBuilderT', fmapInputs, inputstmp)
import qualified Luna.Syntax.Model.Network.Builder.Term.Class    as C -- (NetGraph'', NetLayers, NetCluster, runNetworkBuilderT, fmapInputs, inputstmp)
import           Luna.Syntax.Model.Network.Class                 (Network)
import           Luna.Syntax.Model.Network.Term

import Data.Graph.Backend.VectorGraph

import Control.Monad.Trans.Identity

import Criterion.Main



title s = putStrLn $ "\n" <> "-- " <> s <> " --"



runBuild (g :: NetGraph'') m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers' :<: Draft Static)))
                           $ runNetworkBuilderT' g m


prebuild :: IO (Ref Node (NetLayers' :<: Draft Static), NetGraph'')
prebuild = runBuild def star

--main :: IO ()
--main = do
--    --showcase
--    print "hello2"
--    return ()


-- The function we're benchmarking.
fib m | m < 0     = error "negative!"
      | otherwise = go m
  where
    go 0 = 0
    go 1 = 1
    go n = go (n-1) + go (n-2)

-- Our benchmark harness.
main :: IO ()
main = do
    (_,  g :: NetGraph'') <- prebuild
    return ()
    defaultMain [
        bgroup "single" [ bench "1"  $ nf nodeAdd g
                        --, bench "5"  $ whnf fib 5
                        --, bench "9"  $ whnf fib 9
                        --, bench "11" $ whnf fib 11
                        ]
        ]

--foo :: _ => _
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

nodeAdd :: NetGraph'' -> (Ref Node (NetLayers' :<: Draft Static), NetGraph'')
nodeAdd g = runIdentity $ runNetworkBuilderT' g $ do
    s1 <- int 8
    return s1



-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- FIXME W Luna.Syntax.AST.Term.Class

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
