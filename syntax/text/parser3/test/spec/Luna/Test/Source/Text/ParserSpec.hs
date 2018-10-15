{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Luna.Test.Source.Text.ParserSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Data.Graph.Data.Graph.Class        as Graph
import qualified Luna.Pass                          as Pass
import qualified Luna.Pass.Parsing.Parser           as Parser
import qualified Luna.Pass.Scheduler                as Scheduler
import qualified Luna.Syntax.Prettyprint            as Prettyprint
import qualified Luna.Syntax.Text.Parser.Ast        as Ast
import qualified Luna.Syntax.Text.Parser.Ast.Simple as Simple
import qualified Luna.Syntax.Text.Parser.Parser     as Macro
import qualified Luna.Syntax.Text.Parser.Parser     as PP

import Data.Graph.Data.Graph.Class (Graph)
import Data.Text32                 (Text32)
import Luna.Pass                   (Pass)
import Test.Hspec                  (Arg, Example, Expectation, Spec, describe,
                                    xdescribe, it, xit)
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


e_x :: Text -> Text -> IO ()
e_x input output = runPass $ do
    (ir,cs) <- Parser.runWith Macro.unit (convert input)
    let scope = def
    genCode <- Prettyprint.run @Prettyprint.Simple scope ir
    -- putStrLn "!!!!"
    -- putStrLn (convert genCode)
    genCode `shouldBe` output

e :: Text -> Text -> IO ()
e input output = e_x input ("\n" <> output)

e' :: Text -> IO ()
e' input = e input input

ite :: String -> String -> SpecM () ()
ite s out = it s $ e (convert s) (convert out)

functionDefSpec :: Spec
functionDefSpec = describe "function" $ do
    ite "def foo a: b" "def foo a: b"
    ite "def foo a:"   "def foo a: (EmptyExpression)"
    ite "def foo a"    "def foo a: (MissingSection)"
    ite "def foo"      "def foo: (MissingSection)"
    ite "def"          "def (MissingFunctionName): (MissingSection)"
    ite "def Foo a: b" "def (InvalidFunctionName) a: b"
    ite "def + a: b"   "def + a: b"

caseSpec :: Spec
caseSpec = describe "case" $ do
    it "empty"     $ e  "case a of" $ "case a of\n    (EmptyExpression)"
    it "single"    $ e' "case a of\n    a: b"
    it "multiline" $ e' "case a of\n    a: b\n    c: d"
    xit "wrong way" $ e  "case a of\n a b" $ "case a of\n    (CaseWayNotFunction)"

unitSpec :: Spec
unitSpec = describe "unit" $ do
    it "empty" $ e_x "" ""

accSectionSpec :: Spec
accSectionSpec = describe "acc section" $ do
    ite "[1,2,3].each .succ.pred"         "[1, 2, 3] . each .succ.pred"
    ite "[1,2,3].each .succ.pred.foo"     "[1, 2, 3] . each .succ.pred.foo"
    ite "[1,2,3].each .succ.pred.foo.bar" "[1, 2, 3] . each .succ.pred.foo.bar"

debugSpec :: Spec
debugSpec = xdescribe "error" $ it "debug" $ do

    let input = [qqStr|a .succ.pred.foo|]

    putStrLn "\n\n"
    pprint $ PP.evalVersion1With Macro.unit (convert input)
    pprint $ Simple.simplify $ PP.evalVersion1With Macro.unit (convert input)
    e input "x"



    True `shouldBe` False



spec :: Spec
spec = do
    functionDefSpec
    caseSpec
    unitSpec
    accSectionSpec

    debugSpec
