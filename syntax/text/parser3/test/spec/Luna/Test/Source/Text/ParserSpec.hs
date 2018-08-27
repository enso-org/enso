{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Source.Text.ParserSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Data.Graph.Data.Graph.Class as Graph
import qualified Luna.Pass                   as Pass
import qualified Luna.Pass.Parsing.Parser    as Parser
import qualified Luna.Pass.Scheduler         as Scheduler
import qualified Luna.Syntax.Prettyprint     as Prettyprint

import Data.Graph.Data.Graph.Class (Graph)
import Data.Text32                 (Text32)
import Luna.Pass                   (Pass)
import Test.Hspec                  (Arg, Example, Expectation, Spec, describe,
                                    it)
import Test.Hspec.Core             (SpecM)




type OnDemandPass pass =
    ( Typeable pass
    , Pass.Compile Parser.Parser pass (Graph Parser.Parser)
    )

runPass :: ∀ pass. OnDemandPass pass => Pass Parser.Parser pass () -> IO ()
runPass = runPasses . pure

runPasses :: ∀ pass. OnDemandPass pass => [Pass Parser.Parser pass ()] -> IO ()
runPasses passes = Graph.encodeAndEval @Parser.Parser $ Scheduler.evalT $ do
    Parser.registerDynamic @Parser.Parser
    for_ passes $ \pass -> do
        Scheduler.registerPassFromFunction__ pass -- ONLY FOR TEST SPEC
        Scheduler.runPassByType @pass


shouldParseAs :: Text -> Text -> IO ()
shouldParseAs input output = runPass $ do
    (ir,cs) <- Parser.run (convert input)
    let scope = def
    genCode <- Prettyprint.run @Prettyprint.Simple scope ir
    genCode `shouldBe` output


debugSpec :: Spec
debugSpec = describe "error" $ it "x" $ do
    pure () :: IO ()

    shouldParseAs "def" "b"

    True `shouldBe` False



spec :: Spec
spec = do
    debugSpec
