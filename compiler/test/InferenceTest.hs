{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude.Luna                                    hiding (Num)

import           Data.Construction
import           Old.Data.Prop
import           Data.Record                                     hiding (cons)
import           Data.Version.Semantic                           (showVersion, version)
import           Type.Inference

import qualified Luna.Config.Env                      as Env
import           Luna.Compilation.Pass.Inference.Literals        as LiteralsAssignement
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Pretty.GraphViz
import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import           Old.Luna.Syntax.Term.Class                            hiding (source)
import qualified Old.Luna.Syntax.Term.Class                            as Term
import           Data.Graph
import           Data.Graph.Builder
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit

import qualified Data.Graph.Builder.Class               as Graph
import qualified Data.Graph.Backend.NEC                 as NEC


graph1 :: forall term node edge nr er ls m n e c. (term ~ Draft Static
          , node ~ (ls :<: term)
          , edge ~ Link (ls :<: term)
          , nr   ~ Ref Node node
          , er   ~ Ref Edge edge
          , BiCastable     n (ls :<: term)
          , BiCastable     e edge
          , MonadIO       m
          , NodeInferable       m (ls :<: term)
          , TermNode Lit.Star   m (ls :<: term)
          , TermNode Lit.Number m (ls :<: term)
          , TermNode Lit.String m (ls :<: term)
          , TermNode Var        m (ls :<: term)
          , TermNode Acc        m (ls :<: term)
          , TermNode App        m (ls :<: term)
          , Graph.MonadBuilder (Hetero (NEC.Graph n e c)) m
          )
       => m nr
graph1 = do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    s1 <- str "abc"
    s2 <- str "def"
    s3 <- str "ghi"

    accPlus1a  <- acc "+" i1
    appPlus1a  <- app accPlus1a [arg i2]

    accPlus1b  <- acc "+" i3
    appPlus1b  <- app accPlus1b [arg appPlus1a]

    accConc1a  <- acc "++" s2
    appConc1a  <- app accConc1a [arg s1]

    accConc1b  <- acc "++" appConc1a
    appConc1b  <- app accConc1b [arg s3]

    accLen     <- acc "len" appConc1b
    appLen     <- app accLen []

    accPlus2   <- acc "+" appPlus1b
    appPlus2   <- app accPlus2 [arg appLen]

    -- print appPlus2
    -- return i2
    return appPlus2
    -- return i1

prebuild :: IO (Ref Node (NetLayers :<: Draft Static), NetGraph)
prebuild = runBuild def star

runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                             $ runNetworkBuilderT g m

evalBuild = fmap snd ∘∘ runBuild


main :: IO ()
main = do
    (_,  g00 :: NetGraph) <- prebuild
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v
        TypeCheck.runT $ do
            (root,     g01) <- runBuild  g00 graph1
            (literals, g02) <- runBuild  g01 $ LiteralsUtils.run root
            g03             <- evalBuild g02 $ LiteralsAssignement.runPass literals
            renderAndOpen [ ("g1", "g1", g03)
                          ]
    putStrLn "done"
