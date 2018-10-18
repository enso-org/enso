{-# LANGUAGE OverloadedStrings    #-}

{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Source.Text.ParserSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Control.Monad.State.Layered                as State
import qualified Data.Graph.Component.Edge                  as Link
import qualified Data.Graph.Component.Node.Construction     as Term
import qualified Data.Graph.Data                            as Component
import qualified Data.Graph.Data.Graph.Class                as Graph
import qualified Data.Graph.Data.Layer.Layout               as Layout
import qualified Foreign.Marshal.Alloc                      as Mem
import qualified Foreign.Storable                           as Storable
import qualified Language.Symbol.Operator.Assoc             as Assoc
import qualified Language.Symbol.Operator.Prec              as Prec
import qualified Luna.IR                                    as IR
import qualified Luna.IR.Layer                              as Layer
import qualified Luna.Pass                                  as Pass
import qualified Luna.Pass.Attr                             as Attr
import qualified Luna.Pass.Scheduler                        as Scheduler
import qualified Luna.Syntax.Text.Parser.Ast                as Ast
import qualified Luna.Syntax.Text.Parser.Ast.Class          as Ast
import qualified Luna.Syntax.Text.Parser.Ast.CodeSpan       as CodeSpan
import qualified Luna.Syntax.Text.Parser.Ast.Simple         as Simple
import qualified Luna.Syntax.Text.Parser.Hardcoded          as Hardcoded
import qualified Luna.Syntax.Text.Parser.Lexer              as Parsing
import qualified Luna.Syntax.Text.Parser.Lexer              as Lexer
import qualified Luna.Syntax.Text.Parser.Lexer.Names        as Name
import qualified Luna.Syntax.Text.Parser.Parser             as Parser
import qualified Luna.Syntax.Text.Parser.Parser.ExprBuilder as ExprBuilder
import qualified Luna.Syntax.Text.Parser.State.Version      as Syntax
import qualified Luna.Syntax.Text.Scope                     as Scope
import qualified OCI.Data.Name                              as Name

import Data.Graph.Data.Graph.Class          (Graph)
import Data.Text.Position                   (Delta)
import Data.Text32                          (Text32)
import Luna.Pass                            (Pass)
import Luna.Syntax.Text.Parser.Ast.CodeSpan (CodeSpan)
import OCI.Data.Name                        (Name)
-- import Luna.Syntax.Text.Parser.Pass.Class    (IRBS, Parser)
import Luna.Syntax.Text.Scope  (Scope)
import Luna.Syntax.Text.Source (Source)
import OCI.IR.Link.Class       (type (*-*), Link)
import Test.Hspec              (Arg, Example, Expectation, Spec, describe,
                                xdescribe, it)
import Test.Hspec.Core         (SpecM)

import Luna.IR.Term.Ast.Invalid (adjacentOperators, assocConflict,
                                 emptyExpression, missingRelation,
                                 missingSection, noAssoc, unexpectedSuffix, 
                                 stringNoClosingMark)



-- import qualified Luna.Syntax.Prettyprint                     as Prettyprint


-- | The following is a hspec util allowing us to skip the "it" keyword
instance (t ~ Arg a, Example a, x ~ ())
      => IsString (a -> SpecM t x) where
    fromString = it


-----------------------
-- === Test pass === --
-----------------------

-- === API === --

-- type OnDemandPass pass = (Typeable pass, Pass.Compile Parser.Parsing pass (Graph Parser.Parsing))

-- runPass :: ∀ pass. OnDemandPass pass => Pass Parser.Parsing pass () -> IO ()
-- runPass = runPasses . pure

-- runPasses :: ∀ pass. OnDemandPass pass => [Pass Parser.Parsing pass ()] -> IO ()
-- runPasses passes = Graph.encodeAndEval @Parser.Parsing $ Scheduler.evalT $ do
--     Parser.registerDynamic @Parser.Parsing
--     for_ passes $ \pass -> do
--         Scheduler.registerPassFromFunction__ pass -- ONLY FOR TEST SPEC
--         Scheduler.runPassByType @pass


--         -- runParser__ :: ParserPass (Pass stage Parser)
--         -- => Parsing.Parser Ast -> Text32 -> Pass stage Parser (SomeTerm, Marker.TermMap)

-- shouldParseAs :: Parsing.SyntaxVersion -> Parsing.Parser Ast -> Text -> Text
--               {- -> (Delta, Delta)-} -> IO ()
-- shouldParseAs sv parser input output {-desiredSpan-} = runPass $ do
--     (ir,cs) <- Parser.runParser__ sv parser (convert input)
--         -- irb   <- parser
--         -- scope <- State.get @Scope
--         -- let Parser.IRBS (Parser.IRB irx) = irb
--         --     irb' = Parser.IRBS $ Parser.IRB $ do
--         --         ir <- irx
--         --         cs <- Layer.read @CodeSpan ir
--         --         pure (ir,cs)
--         -- pure $ (,scope) <$> irb'
--         -- pure $ (,scope) <$> irb
--     let scope = def
--     genCode <- Prettyprint.run @Prettyprint.Simple scope ir

--     -- let span = convert $ view CodeSpan.realSpan cs :: (Delta,Delta)
--     genCode `shouldBe` output
--     -- span `shouldBe` desiredSpan

-- printCodePure :: Ast -> IO ()
-- printCodePure ast {-desiredSpan-} = runPass $ do
--     (ir,cs) <- Parser.runMeDebug ast
--         -- irb   <- parser
--         -- scope <- State.get @Scope
--         -- let Parser.IRBS (Parser.IRB irx) = irb
--         --     irb' = Parser.IRBS $ Parser.IRB $ do
--         --         ir <- irx
--         --         cs <- Layer.read @CodeSpan ir
--         --         pure (ir,cs)
--         -- pure $ (,scope) <$> irb'
--         -- pure $ (,scope) <$> irb
--     let scope = def
--     genCode <- Prettyprint.run @Prettyprint.Simple scope ir

--     -- let span = convert $ view CodeSpan.realSpan cs :: (Delta,Delta)
--     print genCode
--     -- span `shouldBe` desiredSpan

-- -- runParser :: Parsing.SyntaxVersion -> Text -> IO ()
-- -- runParser sv input = runPass $ do
-- --     exprs <- Parser.run2 sv (convert input)
-- --     ExprBuilder.run exprs
-- --     pure ()

-- shouldParseItself :: Parsing.SyntaxVersion -> Parsing.Parser Ast -> Text {- -> (Delta, Delta)-} -> IO ()
-- shouldParseItself sv parser input = shouldParseAs sv parser input input

-- unitAs     = shouldParseAs     Parsing.unit'
-- unit       = shouldParseItself Parsing.unit'
-- unit_n   s = unitAs_n s s
-- unitAs_n s = unitAs s . ("\n" <>)
-- exprAs     = shouldParseAs     Parsing.Syntax1 Parsing.expr
-- expr       = shouldParseItself Parsing.Syntax1 Parsing.expr

-- TODO: rename Lexer.Token -> Spanned Ast !
testCase :: Parser.Parser Lexer.Token -> String -> Simple.Ast -> IO ()
testCase p src out = sast `shouldBe` out where
    sast = Simple.simplify ast
    ast  = flip Parser.evalVersion1With (convert src) $ do
        let rplus  = ">>+" :: Name
            noplus = ">+<" :: Name
        Assoc.write Assoc.Right rplus
        Assoc.write Assoc.None  noplus
        Prec.writeRel EQ ("+" :: Name) rplus
        Prec.writeRel EQ ("+" :: Name) noplus
        p

e, u :: String -> Simple.Ast -> IO ()
e = testCase Parser.expr
u = testCase Parser.unit

e' :: String -> IO ()
e' src = e src (convert src)

it_e :: String -> Simple.Ast -> SpecM () ()
it_e s t = it (show s) (e s t)

it_e' :: String -> SpecM () ()
it_e' s = it_e s (convert s)



-----------------------
-- === Ast utils === --
-----------------------

__ :: Convertible' Simple.Ast a => a
__ = convert' Simple.Missing

block (a:as) = Simple.Block (a :| as)

eq :: Simple.Ast -> Simple.Ast -> Simple.Ast
eq = flip Simple.InfixApp "#=#"

secR :: Convertible' Simple.Ast a => Simple.Ast -> Simple.Ast -> a
secR = convert' .: Simple.SectionRight

secL :: Convertible' Simple.Ast a => Simple.Ast -> Simple.Ast -> a
secL = convert' .: Simple.SectionLeft

(<|) :: Convertible' Simple.Ast a => Simple.Ast -> Simple.Ast -> a
(|>) :: Convertible' Simple.Ast a => Simple.Ast -> Simple.Ast -> a
(<|) = secL
(|>) = secR


_x :: Convertible' Simple.Ast a
    => Simple.Ast -> Simple.Ast -> Simple.Ast -> a
_x = convert' .:. Simple.InfixApp

(@.), (@|), (@::), (@:), (@=) :: Convertible' Simple.Ast a
    => Simple.Ast -> Simple.Ast -> a
(@.)  = flip _x "." 
(@|)  = flip _x "," 
(@::) = flip _x "::"
(@:)  = flip _x ":"
(@=)  = flip _x "=" 



----------------------
-- === Literals === --
----------------------

identSpec :: Spec
identSpec = describe "identifier" $ do

  describe "variable" $ do
    it_e' "var"
    it_e' "_var"
    it_e' "var'"
    it_e' "var''"
    it_e "var'o"  $ unexpectedSuffix 1
    it_e "var_a"  $ unexpectedSuffix 2
    it_e "var⸗"   $ unexpectedSuffix 1

  describe "constructor" $ do
    it_e' "Cons"
    it_e' "Cons'"
    it_e' "Cons''"
    "unicode var" $ e' "фываΧξωβ김동욱"
    it_e "Cons'o" $ unexpectedSuffix 1
    it_e "Cons_a" $ unexpectedSuffix 2
    it_e "Cons⸗"  $ unexpectedSuffix 1

  describe "operator" $ do
    it_e' "+"
    it_e' "++"
    it_e' "."
    it_e' ".."
    it_e "..." $ unexpectedSuffix 1
    it_e "..+" $ unexpectedSuffix 1
    it_e "+."  $ unexpectedSuffix 1
    it_e ".+"  $ secR "." (Simple.Var "+")

  describe "other" $ do
    "wildcard" $ e "_" $ Simple.Wildcard


literalNumberSpec :: Spec
literalNumberSpec = describe "number" $ do
    let biggerThanInt64   = (show (maxBound :: Int64)) <> "0"
    it "positive"     $ e' "1"
    it "negative"     $ e  "a -1"          $ "a" (secR Name.uminus 1)
    it "zero pxd"     $ e' "01"
    it "real"         $ e  "1.23"          $ _x 1 "." 23
    it "spaced"       $ e  "123 456"       $ 123 456
    it "spaced reals" $ e  "123 456 . 789" $ _x (123 456) "." 789
    it "int > 64 bit" $ e' biggerThanInt64
    it "invalid sfx"  $ e "1a"             $ unexpectedSuffix 1

literalStringSpec :: Spec
literalStringSpec = describe "string" $ do
  describe "raw" $ do
    "empty"               $ e "''"         $ Simple.Str []
    "simple"              $ e "'a'"        $ Simple.Str ["a"]
    "simple spaced"       $ e "' a '"      $ Simple.Str [" a "]
    "3-quoted"            $ e "'''a'''"    $ Simple.Str ["a"]
    "quote in 3-quoted"   $ e "''' ' '''"  $ Simple.Str [" ' "]
    "empty not closed"    $ e "'"          $ stringNoClosingMark (Simple.Str [])
    "not closed"          $ e "'a"         $ stringNoClosingMark (Simple.Str ["a"])
    "invalid multiline 1" $ e "'a\n b'"    $ stringNoClosingMark (Simple.Str ["a"]) "b'"
    "invalid multiline 2" $ e "'a\nb'"     $ stringNoClosingMark (Simple.Str ["a"])
    "multiline 1"         $ e "'\n a'"     $ Simple.Str ["a"]
    "multiline 2"         $ e "'\n a\n  b'" $ Simple.Str ["a\n b"]
    -- "simple"            $ e [s|"test sentence"|]
    -- "tripple d-quoted"  $ e [s|"""Test"""|] [s|"Test"|]
    -- "escape quote"      $ e [s|"foo\""|]
    -- "escape escape"     $ e [s|"foo\\"|]
    -- "implicite escape"  $ e [s|"foo\bar"|] [s|"foo\\bar"|]

--     describe "interpolated" $ do
--         it "empty"            $ expr [s|''|]
--         it "simple"           $ expr [s|'Test'|]
--         it "simple term 1"    $ expr [s|'foo `2`'|]
--         it "simple term 2"    $ expr [s|'foo `2 + 2`'|]
--         it "escape quote"     $ expr [s|'foo\''|]
--         it "integer escape"   $ exprAs [s|'foo\100ar'|]  [s|'foodar'|]
--         it "escape e-1"       $ exprAs [s|'foo\nbar'|]   [s|'foo\nbar'|]
--         it "escape e-2"       $ exprAs [s|'foo\BSbar'|]  [s|'foo\bbar'|]
--         it "escape e-3"       $ exprAs [s|'foo\NULbar'|] [s|'foo\NULbar'|]
--         it "wrong escape e-1" $ exprAs [s|'foo\Nbar'|]   [s|Invalid Literal (String EscapeCode)|]
--         -- it "not closed"       $ exprAs [s|'foo|]         [s|Invalid Literal (String EscapeCode)|]
--         -- it "multiline"        $ exprAs "a = 'foo\nbar'"         [s|Invalid Literal (String EscapeCode)|]
--         -- it "escaping newline"  $ exprAs [s|'\n'|] "\"\n\""              -- [(0,4)]
--         --     it "tripple single-quoted oneliner"             $ do shouldParseAs expr     "'''The quick brown fox jumps over the lazy dog'''"    "'The quick brown fox jumps over the lazy dog'"
--         --     it "multiline string with inline start"         $ do shouldParseAs expr     "'The quick \n brown fox jumps over the lazy dog'"     "'The quick \nbrown fox jumps over the lazy dog'"
--         --     it "multiline string with non-indented newline" $ do shouldParseAs expr     "'The quick \n\n brown fox jumps over the lazy dog'"   "'The quick \n\nbrown fox jumps over the lazy dog'"
--         --     it "multiline string with space-only line"      $ do shouldParseAs expr     "'The quick \n  \n brown fox jumps over the lazy dog'" "'The quick \n \nbrown fox jumps over the lazy dog'"
--         --     it "multiline string with newline start"        $ do shouldParseAs expr     "'\nThe quick\nbrown fox jumps over the lazy dog\n'"   "'The quick\nbrown fox jumps over the lazy dog'"
--         --     it "simple interpolated strings"                $ do shouldParseItself expr "'The quick brown fox jumps over {2 + 2} lazy dogs'"


listLikeSpec :: String -> String -> Spec
listLikeSpec open close = describe "list" $ do
    let ee s r = e (open <> s <> close) $ convert (open <> "_" <> close) r
    "empty"          $ ee ""       $ []
    "singular"       $ ee "a"      $ ["a"]
    "double"         $ ee "a b, c" $ ["a" "b", "c"]
    "head section"   $ ee ",a"     $ [__, "a"]
    "tail section"   $ ee "a,"     $ ["a", __]
    "double section" $ ee ","      $ [__, __]
    "triple section" $ ee ",,"     $ [__, __, __]
    "side section"   $ ee ",a,"    $ [__, "a", __]

literalListSpec :: Spec
literalListSpec = listLikeSpec "[" "]"

literalTupleSpec :: Spec
literalTupleSpec = listLikeSpec "(" ")"

literalSpec :: Spec
literalSpec = describe "literal" $ do
    literalNumberSpec
    literalStringSpec
    literalListSpec
    literalTupleSpec



-----------------------
-- === Operators === --
-----------------------

operatorSpec :: Spec
operatorSpec = describe "operator" $ do

  describe "simple" $ do
    "single"          $ e "a - b"         $ "a" - "b"
    "modifier"        $ e "a += 1"        $ _x "a" "+=" 1
    "application"     $ e "a b"           $ "a" "b"
    "comma"           $ e "a,b,c"         $ ("a" @| "b") @| "c"
    "typed"           $ e "a :: b"        $ "a" @:: "b"

  describe "section" $ do
    "line right"      $ e "+ a"           $ "+" |> "a"
    "line left"       $ e "a +"           $ "a" <| "+"
    "glued right"     $ e "+a"            $ "+" |> "a"
    "glued left"      $ e "a+"            $ "a" <| "+"
    "glued right app" $ e "a +b * c"      $ "a" ("+" |> "b") * "c"
    "glued left app"  $ e "a b+ * c"      $ "a" ("b" <| "+") * "c"
    "glued lr app"    $ e "a *b+ c"       $ "a" (("*" |> "b") <| "+") "c"
    "lens app"        $ e "a .b.c"        $ "a" (("." |> "b") @. "c")
    "hack app"        $ e "a . b.c 4"     $ ("a" @. "b" @. "c") 4 -- see Luna.Syntax.Text.Parser.Parser.ExprBuilder.hackApp

  describe "accessors" $ do
    "non spaced"      $ e "a.b"           $ "a" @. "b"
    "spaced"          $ e "a . b"         $ "a" @. "b"
    "spaced arg"      $ e "a . b c"       $ ("a" @. "b") "c"
    "spaced args"     $ e "a . b c d"     $ ("a" @. "b") "c" "d"
    "spaced nested"   $ e "a . b c . d e" $ ((("a" @. "b") "c") @. "d") "e"
    "spaced nested 2" $ e "a b . c . d e" $ (("a" "b" @. "c") @. "d") "e"
    "complex 1"       $ e "a b.c . d e"   $ (("a" ("b" @. "c")) @. "d") "e"
    "complex 2"       $ e "a b. c . d e"  $ (("a" ("b" <| ".") "c") @. "d") "e"
    "complex 3"       $ e "a b .c . d e"  $ ("a" "b" ("." |> "c") @. "d") "e"
    "complex 4"       $ e "a b .c d . e"  $ "a" "b" ("." |> "c") "d" @. "e"
    "complex 5"       $ e "a b .c d. e"   $ "a" "b" ("." |> "c") ("d" <| ".") "e"
    "complex 6"       $ e "a b .c. d"     $ "a" "b" (("." |> "c") <| ".") "d"

  describe "precedence" $ do
    "simple"          $ e "a + b * c"     $ "a" + ("b" * "c")
    "spaced"          $ e "a+b * c"       $ ("a" + "b") * "c"

  describe "assignment" $ do
    "empty"           $ e "a ="           $ "a" `eq` emptyExpression
    "simple"          $ e "a = b"         $ "a" `eq` "b"
    "operator"        $ e "a = +"         $ "a" `eq` "+"
    "left section"    $ e "a = b +"       $ "a" `eq` ("b" <| "+")
    "right section"   $ e "a = + b"       $ "a" `eq` ("+" |> "b")
    "eq operator"     $ e "a = ="         $ "a" `eq` "="
    "eq left sec"     $ e "a = b ="       $ "a" `eq` ("b" <| "=")
    "eq right sec"    $ e "a = = b"       $ "a" `eq` ("=" |> "b")
    -- "grouped eq lsec"   $ e "(a =)"         $ "(_)" [secL "a" "="]
    -- "grouped eq rsec"   $ e "(= a)"         $ "(_)" [secR "=" "a"]
    "not spaced"      $ e "a=b + c"       $ "a" `eq` ("b" + "c")
    "left spaced"     $ e "a =b + c"      $ "a" `eq` ("b" + "c")
    "right spaced"    $ e "a= b + c"      $ "a" `eq` ("b" + "c")
    "spaced"          $ e "a = b + c"     $ "a" `eq` ("b" + "c")
    "named args"      $ e "a = b x=1"     $ "a" `eq` ("b" (_x "x" "=" 1))
    "pattern"         $ e "V x y z = v"   $ "V" "x" "y" "z" `eq` "v"
    -- "multiline"         $ e "a =\n b"       $ "c"
    -- "multiline eq lsec" $ e "a =\n b ="     $ "c"
    -- "multiline eq rsec" $ e "a =\n = b"     $ "c"
    -- "multiline argval"  $ e "a =\n b = 1"   $ "c"
    -- "multiline argvals" $ e "a =\n b = 1\n c = 2" $ "c"

  describe "multiline" $ do
    "grouping"        $ e "a\n b c"       $ "a" ("b" "c")
    "section 1"       $ e "a +\n b c"     $ "a" + ("b" "c")
    "section 2"       $ e "a+ \n b c"     $ Simple.App ("a" <| "+") ("b" "c")
    "section 3"       $ e "a \n +b c"     $ "a" (("+" |> "b") "c")
    "continuation"    $ e "a \n + b c"    $ "a" + "b" "c"

  describe "invalid" $ do
    "adj infix"       $ e "a + + b"       $ _x "a" adjacentOperators "b"
    "adj infix 2"     $ e "a + + + b"     $ _x "a" adjacentOperators "b"
    "adj postfix"     $ e "a + +"         $ "a" <| adjacentOperators
    "adj prefix"      $ e "+ + a"         $ adjacentOperators |> "a"
    "wrong assoc"     $ e "a + b >>+ c"   $ _x ("a" + "b") assocConflict "c"
    "no prec"         $ e "a + b +++ c"   $ _x ("a" + "b") missingRelation "c"
    "no assoc"        $ e "a >+< b >+< c" $ _x (_x "a" ">+<" "b") noAssoc "c"


mixfixSpec :: Spec
mixfixSpec = describe "groups" $ do
    "empty group"  $ e "()"                 $ "(_)" []
    "group"        $ e "(a b)"              $ "(_)" ["a" "b"]
    "a)"           $ e "a)"                 $ "a" <| ")"
    "nested rules" $ e "(if a then b) else" $ "(_)" ["if_then" "a" "b"] "else"
    "dd"           $ e "if a b then c d"    $ "if_then" ("a" "b") ("c" "d")
    "nested grps"  $ e "[(a,b)]"            $ "[_]" ["(_)" ["a", "b"]]


layoutSpec :: Spec
layoutSpec = describe "layout" $ do
    "broken expr 1"  $ e "if a then b else c"     $ "if_then_else" "a" "b" "c"
    "broken expr 2"  $ e "if a\n then b else c"   $ "if_then_else" "a" "b" "c"
    "broken expr 3"  $ e "if a\n then b\n else c" $ "if_then_else" "a" "b" "c"
    "broken expr 4"  $ e "if a then\n b else c"   $ "if_then_else" "a" "b" "c"
    "broken expr 5"  $ e "if a then b else\n c"   $ "if_then_else" "a" "b" "c"
    "broken expr 6"  $ e "if a then\n b else\n c" $ "if_then_else" "a" "b" "c"
    "broken expr 7"  $ e "if a then\n b else\n c" $ "if_then_else" "a" "b" "c"
    "broken expr 8"  $ e "if a then\n b\n c" $ "if_then" "a" (block ["b", "c"])
    "after operator" $ e "a: b:\n c\n d"  $ "a" @: ("b" @: (block ["c", "d"]))



----------------------
-- === Mixfixes === --
----------------------

-- | Testing all possible missing sections scenarios. We've got in scope
--   definitions of both `if_then_else` as well as `if_then`. We try here all
--   possible combinations of correct and incorrect applications, like
--   `if a then else c` or `if then else c`.
missingSectionsSpec :: Spec
missingSectionsSpec = describe "mixfix" $ let

    tst :: String -> [String] -> [Either Simple.Ast String] -> SpecM () ()
    tst n ns as = it_e (mconcat $ shuffle ns (pat <$> as))
                $ Simple.apps (convert n) (exp <$> as)

    opt   :: String -> [Either Simple.Ast String]
    opt   = \a -> [Right a, Left emptyExpression]
    noSec = Left missingSection
    pat   = either (const " ") (\t -> " " <> t <> " ")
    exp   = either id convert

    shuffle :: [a] -> [a] -> [a]
    shuffle (a:as) (b:bs) = a:b:shuffle as bs
    shuffle _ _           = []

    _ite a b c = tst "if_then_else" ["if", "then", "else"] [a,b,c]
    _it  a b   = tst "if_then"      ["if", "then"        ] [a,b]
    _ie  a   c = tst "if_then_else" ["if", "", "else"    ] [a, noSec, c]
    _i   a     = tst "if_then"      ["if", ""            ] [a, noSec]

    in do
        sequence_ $ _ite <$> opt "a" <*> opt "b" <*> opt "c"
        sequence_ $ _it  <$> opt "a" <*> opt "b"
        sequence_ $ _ie  <$> opt "a" <*> opt "c"
        sequence_ $ _i   <$> opt "a"



-------------------------
-- === Definitions === --
-------------------------

funcDefSpec :: Spec
funcDefSpec = describe "function" $ do
    it_e "def f a: a" $ "def_:" "f" ["a"] "a"
    it_e "def + a: a" $ "def_:" "+" ["a"] "a"
    it_e "def f a:"   $ "def_:" "f" ["a"] emptyExpression
    it_e "def f a"    $ "def_:" "f" ["a"] missingSection
    it_e "def f"      $ "def_:" "f" [] missingSection
    it_e "def"        $ "def_:" emptyExpression [] missingSection


classDefSpec :: Spec
classDefSpec = describe "class" $ do
    it_e "class Foo:" $ "class_:" "Foo" [] emptyExpression

caseSpec :: Spec
caseSpec = describe "case" $ do
    "simple" $ e "case foo x of\n a: b" $ "case_of" ("foo" "x") (block $ _x "a" ":" "b")


importSpec :: Spec
importSpec = describe "import" $ do
    "x" $ e "import Std.Math" $ "import" (_x "Std" "." "Math")
    -- "x" $ e "import Std.Math: Vector Scalar"          $ "x"
    -- "x" $ e "import .Local"                           $ "x"
    -- "x" $ e "import World: Std"                       $ "x"

unitSpec :: Spec
unitSpec = describe "unit" $ do
    "empty" $ u "" (Simple.Unit Simple.Missing)

commentSpec :: Spec
commentSpec = describe "comment" $ do
    "single line" $ e "# comment" $ Simple.Comment "comment"


debugSpec :: Spec
debugSpec = xdescribe "error" $ it "x" $ do
    pure () :: IO ()
    putStrLn "\n"

    -- let input     = "a: b: c + d"
    -- let input     = "a: b: c"
    -- let input     = "a = x: «0» foo b"
    -- let input     = "\171\&0\187def main:\n    None"
    -- let input     = "«0»def main:\n    None"
    -- let input     = "a = b"
    -- src <- readFile "/home/wdanilo/dev/luna3/stdlib/Std/src/Base.luna"
    -- src <- readFile "/tmp/input.luna"

    let
        toks      = Lexer.eval Syntax.Version1 input
        -- stream    = ExprBuilder.buildExprSegment toks
        -- input = convert src -- [qqStr|'x'|]
        input = "class T:\n TC:\n  x :: I "

    putStrLn "\nTOKS:\n"
    pprint toks

    -- putStrLn "\nSTREAM:\n"
    -- pprint stream


    putStrLn "\nRESULT:\n"
    pprint $ Parser.evalVersion1With Parser.unit input
    pprint $ Simple.simplify $ Parser.evalVersion1With Parser.unit input


    -- pprint $ Simple.simplify $ Parser.evalVersion1With Parser.unit (convert src)
    True `shouldBe` False


spec :: Spec
spec = do
    identSpec
    literalSpec
    operatorSpec
    mixfixSpec
    missingSectionsSpec
    layoutSpec
    funcDefSpec
    -- classDefSpecz
    caseSpec
    importSpec
    commentSpec
    unitSpec

    debugSpec



--         describe "units" $ do
--             it "empty unit"                                 $ shouldParseItself' unit' ""                                        []
--             it "unit with newlines on the end"              $ shouldParseAs'     unit' "def foo: bar\n\n\n" "\n<function 'foo'>" [(4,3),(0,12),(0,12),(0,12)]
--             it "unit with newlines on the beginning"        $ shouldParseAs'     unit' "\n\n\ndef foo: bar" "\n<function 'foo'>" [(3,0),(4,3),(0,12),(0,12),(0,15)]

--         describe "classes" $ do
--             it "phantom class"                              $ shouldParseItself'' unit' "class Phantom"                                               [(0,13),(0,13),(0,13)]
--             it "no cons and fields class"                   $ shouldParseAs'      unit' "class C:\n    def foo: a" "\nclass C:\n    <function 'foo'>" [(4,3),(13,10),(0,23),(0,23),(0,23)]
--             it "constructor-only class"                     $ shouldParseItself'' unit' ("class Bool:"
--                                                                                         </> "    True"
--                                                                                         </> "    False"
--                                                                                         ) [(16,4),(5,5),(0,30),(0,30),(0,30)]

--             it "implicit constructor, parametrized class"   $ shouldParseItself'' unit' ("class Vector a:"
--                                                                                         </> "    x y z :: a"
--                                                                                         ) [(13,1),(9,1),(6,10),(0,30),(0,30),(0,30)]

--             -- FIXME [WD]: broken pretty printer here
--             it "complex class"                              $ shouldParse          unit' ("class Vector a:"
--                                                                                         </> "    V a a a"
--                                                                                         </> "    X: fld :: a"
--                                                                                         </> "    def foo: 0"

--         describe "markers" $ do
--             it "marked expression"                       $ shouldParseItself' expr "«0»foo bar" [(0,3),(0,3),(1,3),(0,7),(0,10)]
--             it "marked assignment expression"            $ shouldParseItself' expr "«0»a = foo" [(0,3),(0,1),(3,3),(0,7),(0,10)]


--         describe "metadata" $ do
--             it "metadata line"                           $ shouldParseItself'' unit' "### META {\"0\": {\"studio\": ...}}" [(0,31),(0,31),(0,31)]

--         describe "foreign symbol declaration" $ do
--             it "symbol checking dot spacing"             $ shouldParseAs'     foreignSymbolImport
--                                                                                 "\"alloc_tensor\" allocTensor :: C.Int64 -> C.Ptr"
--                                                                                 "\"alloc_tensor\" allocTensor :: C . Int64 -> C . Ptr"
--                                                                                 [(0,14),(0,1),(0,7),(1,2),(0,10),(0,1),(1,5),(16,16),(0,46)]
--             it "symbol with unspecified safety"          $ shouldParseItself' foreignSymbolImport "\"allocTensor\" allocTensor :: C . Int64 -> C . Ptr" [(0,13),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(16,20),(0,49)]
--             it "symbol with variable for symbol name"    $ shouldParseItself' foreignSymbolImport "myVar myFunc :: C . Ptr" [(0,5),(0,1),(11,7),(0,23)]
--             it "symbol with safety and variable name"    $ shouldParseItself' foreignSymbolImport "safe myVar myFunc :: C . Ptr" [(0,4),(1,5),(0,1),(11,7),(0,28)]
--             it "safe symbol"                             $ shouldParseItself' foreignSymbolImport "safe \"allocTensor\" allocTensor :: C . Int64 -> C . Ptr" [(0,4),(1,13),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(16,20),(0,54)]
--             it "unsafe symbol"                           $ shouldParseItself' foreignSymbolImport "unsafe \"allocTensor\" allocTensor :: C . Int64 -> C . Ptr" [(0,6),(1,13),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(16,20),(0,56)]
--             it "symbol with bad safety specification"    $ shouldParseAs'     foreignSymbolImport
--                                                                                 "foo myVar myFunc :: C . Ptr"
--                                                                                 "Invalid \"Invalid safety specification.\""
--                                                                                 [(0,23)]

--         -- FIXME [Ara, WD] Work out why the pretty printer is putting an extra newline at the end of these.
--         describe "C-FFI Syntax" $ do
--             it "single object file, single import"       $ shouldParseItself' unit' ("foreign import C:"
--                                                                                     </> "    \"tensor.so\":"
--                                                                                     </> "        \"alloc_tensor\" allocTensor :: C . Int64 -> C . Int64\n"
--                                                                                     ) [(0,11),(0,14),(0,1),(0,9),(1,2),(0,12),(0,1),(1,9),(16,22),(10,52),(22,73),(0,95),(0,95)]
--             it "variable for object file"                $ shouldParseItself' unit' ("foreign import C:"
--                                                                                     </> "    tensorObjectName:"
--                                                                                     </> "        \"alloc_tensor\" allocTensor :: C . Int64 -> C . Int64\n"
--                                                                                     ) [(0,16),(0,14),(0,1),(0,9),(1,2),(0,12),(0,1),(1,9),(16,22),(10,52),(22,78),(0,100),(0,100)]
--             it "import something other than C"           $ shouldParseItself' unit' ("foreign import Js:"
--                                                                                     </> "    \"tensor.so\":"
--                                                                                     </> "        \"alloc_tensor\" allocTensor :: C . Int64 -> C . Int64\n"
--                                                                                     ) [(0,11),(0,14),(0,1),(0,9),(1,2),(0,12),(0,1),(1,9),(16,22),(10,52),(23,73),(0,96),(0,96)]
--             it "single object file, multiple symbols"    $ shouldParseItself' unit' ("foreign import C:"
--                                                                                     </> "    \"tensor.so\":"
--                                                                                     </> "        \"alloc_tensor\" allocTensor :: C . Int64 -> C . Ptr"
--                                                                                     </> "        \"free_tensor\" freeTensor :: C . Int64 -> C . Ptr\n"
--                                                                                     ) [(0,11),(0,14),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(16,20),(10,50),(0,13),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(15,20),(9,48),(22,128),(0,150),(0,150)]
--             it "multiple object files, single symbol"    $ shouldParseItself' unit' ("foreign import C:"
--                                                                                     </> "    \"tensor.so\":"
--                                                                                     </> "        \"alloc_tensor\" allocTensor :: C . Int64 -> C . Ptr"
--                                                                                     </> "    \"foo.dylib\":"
--                                                                                     </> "        \"init_foo\" initFoo :: C . UInt32 -> C . UInt32 -> C . Ptr\n"
--                                                                                     ) [(0,11),(0,14),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(16,20),(10,50),(22,71),(0,11),(0,10),(0,1),(0,10),(1,2),(0,13),(0,1),(1,10),(0,24),(1,2),(0,27),(0,1),(1,7),(12,35),(10,57),(5,78),(0,176),(0,176)]
--             it "multiple object files, multiple symbols" $ shouldParseItself' unit' ("foreign import C:"
--                                                                                     </> "    \"tensor.so\":"
--                                                                                     </> "        \"alloc_tensor\" allocTensor :: C . Int64 -> C . Ptr"
--                                                                                     </> "        \"free_tensor\" freeTensor :: C . Int64 -> C . Ptr"
--                                                                                     </> "    \"foo.dylib\":"
--                                                                                     </> "        \"init_foo\" initFoo :: C . UInt32 -> C . UInt32 -> C . Ptr\n"
--                                                                                     ) [(0,11),(0,14),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(16,20),(10,50),(0,13),(0,1),(0,9),(1,2),(0,12),(0,1),(1,7),(15,20),(9,48),(22,128),(0,11),(0,10),(0,1),(0,10),(1,2),(0,13),(0,1),(1,10),(0,24),(1,2),(0,27),(0,1),(1,7),(12,35),(10,57),(5,78),(0,233),(0,233)]

-- -- foo . pos . x += 1
-- --           . y += 2

