{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds     #-}

module Main where

import           Data.Graph
import           Data.Graph.Builders
import           Prologue                                        hiding (Version, cons, read, ( # ), Num, Cons)

import           Text.Printf                                     (printf)
import qualified Control.Monad.Writer                            as Writer
import           Control.Monad.Event
import           Data.Attr                                       (attr)
import           Data.Construction
import           Data.Container                                  (elems, index_)
import           Data.Container                                  hiding (impossible)
import           Data.Graph.Builder
import           Data.Graph.Query                                hiding (Graph)
import qualified Data.Graph.Query                                as Sort
import           Data.Index                                      (idx)
import           Data.Layer.Cover
import qualified Data.Map                                        as Map
import           Data.Prop
import           Data.Record                                     hiding (Layout, Cons, cons)
import           Data.Version.Semantic
import           Development.Placeholders
import           Type.Inference

import qualified Luna.Config.Env                      as Env
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Inference.Unification     (UnificationPass (..))
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import qualified Luna.Runtime.Dynamics                         as Runtime
import qualified Luna.Syntax.Term.Format                           as EvalModel
import qualified Luna.Library.Symbol                       as Symbol
import           Luna.Syntax.Term.Expr                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target, Input)
import qualified Luna.Syntax.Term.Expr                            as Term
import qualified Luna.Syntax.Term.Lit                        as Lit
import           Data.Graph.Builder.Ref                          as Ref
import qualified Data.Graph.Builder.Class                        as Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetNode, NetCluster, runNetworkBuilderT, runNetworkBuilderT2, fmapInputs, inputstmp)
import           Luna.Syntax.Model.Network.Class                 (Network)
import           Luna.Syntax.Model.Network.Term

import qualified Data.Graph.Backend.NEC as NEC
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



main :: IO ()
main = do
    E.main
    (_,  g :: NetGraph ) <- prebuild
    renderAndOpen [ ("xg", "xg", g)
                  ]
