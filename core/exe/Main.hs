{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}

module Main where


import Data.Graph
import Data.Graph.Builders
import Prologue            hiding (Cons, Num, Version, cons, read, ( # ))

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
import           Data.Prop
import           Data.Record              hiding (Cons, Layout, cons)
import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import qualified Data.Graph.Builder.Class                        as Graph
import           Data.Graph.Builder.Ref                          as Ref
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import qualified Luna.Config.Env                                 as Env
import qualified Luna.Library.Symbol                             as Symbol
import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                           (Dynamic, Static)
import qualified Luna.Runtime.Dynamics                           as Runtime
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetLayers, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Luna.Syntax.Model.Network.Class                 (Network)
import           Luna.Syntax.Model.Network.Term
import           Luna.Syntax.Term.Expr                           hiding (Draft, Expr, Input, Lit, Source, Target, Thunk, Val, source, target)
import qualified Luna.Syntax.Term.Expr                           as Term
import qualified Luna.Syntax.Term.Format                         as EvalModel
import qualified Luna.Syntax.Term.Lit                            as Lit

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast (Edge e) = cast e
--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast e = Edge $ cast e
-- --------------------------------------
--  !!! KEEP THIS ON THE BEGINNING !!! --
-- --------------------------------------
-- - vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ---

prebuild :: IO (Ref Node (NetLayers :<: Draft Static), NetGraph)
prebuild = runBuild2 def $ do
    star
    star
    star




runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                             $ runNetworkBuilderT g m


runBuild2 (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                             $ runNetworkBuilderT2 g m

evalBuild = fmap snd ∘∘ runBuild
--TODO: test1
-- TODO: test2

main :: IO ()
main = do
    E.main
    (_,  g :: NetGraph ) <- prebuild
    renderAndOpen [ ("xg", "xg", g)
                  ]
