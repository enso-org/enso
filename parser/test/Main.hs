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
import           Luna.Parser.State      (ParserState)
import           Text.Trifecta.Combinators (DeltaParsing)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

runBuild (g :: NetGraph a) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers a :<: Draft Static)))
                             $ runNetworkBuilderT g m


evalBuild = fmap snd ∘∘ runBuild

main :: IO ()
main = do
    print "Parser test!"
    --let (_, g :: NetGraph ()) = runIdentity testbuild
    --renderAndOpen [("g1", "g1", g)]

    --let out = parseString "'hello'" testparse1
    case xxx of
        Left d -> print d
        Right (n,g) -> renderAndOpen [("g1", "g1", g)]
    return ()

--tst = parseString "'hello'" Lit.string



--input1 :: ( term ~ Draft Static
--            , nr   ~ Ref Node (ls :<: term)
--            , MonadIO       m
--            , NodeInferable m (ls :<: term)
--            , TermNode Lit.String m (ls :<: term)
--            , TermBuilder Lit.String m (ls :<: term)
--            ) => m ()
input1 = do
    s1  <- str' "Str!"
    return s1

input_parse1 = PLit.string

--input_parse1' :: _ => _
input_parse1' = parseGen PLit.string Parser.defState

input_parse2' :: (DeltaParsing f, TermBuilder Lit.String m a) => f (m a, ParserState)
input_parse2' = parseGen PLit.string2 Parser.defState

--tst = evalBuild def input1 :: NetGraph ()


--testbuild :: (MonadFix m, _)
--          => m (Ref Node (NetLayers () :<: Draft Static), NetGraph ())
testbuild = runBuild (def :: NetGraph ()) input1

--testparse1 :: _ => m ((Ref Node (NetNode ()), ParserState), NetGraph ()) -- m (Ref Node (NetLayers () :<: Draft Static), NetGraph ())
testparse1 = runBuild (def :: NetGraph ()) input_parse1'

--testparse2 :: (DeltaParsing f, TermBuilder Lit.String m a) => f ((m a, ParserState), NetGraph ())
testparse2 :: DeltaParsing f => f (Ref Node (NetNode ()), NetGraph ())
testparse2 = runIdentity ∘ runBuild (def :: NetGraph ()) ∘ fst <$> input_parse2'


xxx :: Either Doc (Ref Node (NetNode ()), NetGraph ())
xxx = parseString "'hello'" testparse2
