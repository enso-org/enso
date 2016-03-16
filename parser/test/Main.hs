{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
--{-# LANGUAGE PartialTypeSignatures     #-}


module Main where

import Prologue
import Luna.Parser.Parser
import qualified Luna.Syntax.AST.Term.Lit as Lit
import qualified Luna.Parser.Literal      as PLit
import Luna.Syntax.Model.Network.Builder.Term hiding (runNetworkBuilderT)
import Luna.Syntax.Model.Network.Term
import Luna.Evaluation.Runtime
import Luna.Syntax.Model.Network.Builder.Node (NodeInferable, TermNode)
import Data.Graph
import           Luna.Syntax.Model.Layer        ((:<:))
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetCluster, fmapInputs, inputstmp)
import qualified Luna.Syntax.Model.Network.Builder.Term.Class as Term
import           Type.Inference
import           Luna.Diagnostic.Vis.GraphViz
import           Control.Monad.Event (Dispatcher)
import           Control.Monad.Identity hiding (when)
import qualified Luna.Parser.Parser     as Parser
import qualified Luna.Parser.State      as Parser
import           Text.Trifecta.Combinators (DeltaParsing)
import           Text.Parser.Combinators (eof)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Luna.Parser.Term as Term
import           Luna.Parser.Class        (ASTParser, ASTParserCore, ASTBuilderCtx)
import qualified Luna.Syntax.Model.Text.Location as Location
import Luna.Parser.Class (Parser)
import Luna.Syntax.Model.Network.Builder.Class (NetworkBuilderT, runNetworkBuilderT)
--import qualified Luna.Parser.Function as Func


runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                           $ flip Location.evalT Nothing
                           $ Term.runNetworkBuilderT g
                           $ runNetworkBuilderT
                           $ m


evalBuild = fmap snd ∘∘ runBuild




input = "foo @ (Vector x y z) = v"

myparser :: (ASTParserCore p m a, p ~ Parser) => p (m a, Parser.State)
myparser = parseGen PLit.string Parser.defState

myparser2 :: (ASTParserCore p m a, p ~ Parser) => p (m a, Parser.State)
myparser2 = parseGen Term.assignment Parser.defState


parsed :: ASTBuilderCtx m a => Either Doc (m a, Parser.State)
parsed = parseString input myparser2

main1 :: IO ()
main1 = do
    print "Parser test!"
    case parsed of
        Left  d          -> print d
        Right (bldr, ps) -> do
            print "ok"
            (_ :: Ref Node NetNode, g :: NetGraph) <- runBuild (def :: NetGraph) bldr
            print "ok"
            --renderAndOpen [("g1", "g1", g)]
    return ()




input2 = [s|def if_then_else cond ok fail :
    "foo"
    "bar"
i
|]

input3 = [s|a . foo . bar + b
|]


--myparser3 :: ASTParserCore p m a => p (m a, Parser.State)
myparser3 = parseGen Term.s1_function' Parser.defState
myparser4 = parseGen Term.expr   Parser.defState

--parsed2 :: ASTBuilderCtx m a => Either Doc (m a, Parser.State)
--parsed2 :: _ => _
parsed2 = parseString input3 myparser4

data Test a = Test { _showGraph :: Bool, _desc :: String, _content :: a} deriving (Show)
makeLenses ''Test


inputs :: [Test String]
inputs  = [ Test False "Variables"          "ala"
          , Test False "Constructors"       "True"
          , Test False "Operators"          "+"
          , Test False "String literals"    "\"test\""
          , Test False "Applications"       "foo bar baz"
          , Test False "Accessors"          "foo.bar"
          , Test False "Nested accessors"   "foo.bar.baz"
          ]

checkResult (Test draw name res) = (putStrLn ∘ ((name <> ": ") <>)) =<< resDesc where
    resDesc = case res of
        Left e           -> return $ "error: \n" <> show e
        Right (bldr, ps) -> do
            (a, g :: NetGraph) <- runBuild (def :: NetGraph) bldr
            when draw $ renderAndOpen [(name, name, g)]
            return "ok"



partialInput = [s|+|]

partialParsed = parseString partialInput partialParser
partialParser = parseGen (Term.partial <* eof) Parser.defState


mainPartial :: IO ()
mainPartial = do
    print "Partial parser test!"
    case partialParsed of
        Left  d          -> print d
        Right (bldr, ps) -> do
            (a, g :: NetGraph) <- runBuild (def :: NetGraph) bldr
            renderAndOpen [("g1", "g1", g)]
            print a
            print "ok"
    return ()

mainLam :: IO ()
mainLam = do
    print "Parser test!"
    case parsed2 of
        Left  d          -> print d
        Right (bldr, ps) -> do
            (a, g :: NetGraph) <- runBuild (def :: NetGraph) bldr
            renderAndOpen [("g1", "g1", g)]
            print a
            print "ok"
    return ()


main :: IO ()
main = do
    let results = (content %~ flip parseString partialParser) <$> inputs
    mapM_ checkResult results
    --mainLam

