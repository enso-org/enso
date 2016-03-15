{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

module Main where

import Prelude.Luna
import qualified Data.Graph as Graph
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





addNodeEnv = do
    (_,  g :: NetGraph'') <- prebuild
    let n = fst $ nodeAdd g
    return (g, n)


main :: IO ()
main = do
    (_,  g :: NetGraph'') <- prebuild
    n <- evaluate $ force $ fst $ nodeAdd g
    return ()

    defaultMain
        [ env addNodeEnv $ \ ~(g,n) ->
            bgroup "add nodes" $ take 6 $ benchNodeAdd g n <$> doubles 100
        ]





benchNodeAdd :: NetGraph'' -> NetLayers' :<: Draft Static -> Int -> Benchmark
benchNodeAdd g n i = bench (show i) $ nf (flip (foldl' (flip ($))) (replicate i (nodeAddPure_ n))) g

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

nodeAdd_ :: NetGraph'' -> NetGraph''
nodeAdd_ = snd ∘ nodeAdd

nodeAdd :: NetGraph'' -> (NetLayers' :<: Draft Static, NetGraph'')
nodeAdd g = runIdentity $ runNetworkBuilderT' g $ do
    s1   <- int 8
    s1_v <- read s1
    return s1_v



-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- FIXME W Luna.Syntax.AST.Term.Class

-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
-- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
