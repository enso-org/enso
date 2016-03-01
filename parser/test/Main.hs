{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE PartialTypeSignatures     #-}


module Main where

import Prologue
import Luna.Parser.Parser
import qualified Luna.Syntax.AST.Lit as Lit
import qualified Luna.Parser.Literal as PLit
import Luna.Syntax.Model.Network.Builder.Term
import Luna.Syntax.Model.Network.Term
import Luna.Evaluation.Runtime
import Luna.Syntax.Model.Network.Builder.Node (NodeInferable, TermNode)
import Data.Graph
import           Luna.Syntax.Model.Layer        ((:<:))
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetCluster, runNetworkBuilderT, fmapInputs, inputstmp)
import           Type.Inference
import           Luna.Diagnostic.Vis.GraphViz
import           Control.Monad.Event (Dispatcher)
import           Control.Monad.Identity
import qualified Luna.Parser.Parser     as Parser
import qualified Luna.Parser.State      as Parser
import           Text.Trifecta.Combinators (DeltaParsing)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Luna.Parser.Term (T)
import qualified Luna.Parser.Term as Term

runBuild (g :: NetGraph a) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers a :<: Draft Static)))
                             $ runNetworkBuilderT g m


evalBuild = fmap snd ∘∘ runBuild

main :: IO ()
main = do
    print "Parser test!"
    case parsed of
        Left  d          -> print d
        Right (bldr, ps) -> do
            (_ :: Ref Node (NetNode ()), g :: NetGraph ()) <- runBuild (def :: NetGraph ()) bldr
            renderAndOpen [("g1", "g1", g)]
    return ()


input = "v = x y z"

myparser :: (DeltaParsing p, TermBuilder Lit.String m a) => p (m a, Parser.State)
myparser = parseGen PLit.string Parser.defState

myparser2 :: (DeltaParsing p, T m a) => p (m a, Parser.State)
myparser2 = parseGen Term.assignment Parser.defState


parsed :: T m a => Either Doc (m a, Parser.State)
parsed = parseString input myparser2
