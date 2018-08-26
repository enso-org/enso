{-# LANGUAGE OverloadedStrings    #-}

{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Test.Source.Text.ParserSpec where

import Prologue
import Test.Hspec.Expectations.Lifted

import qualified Control.Monad.State.Layered                 as State
import qualified Data.Graph.Component.Edge                   as Link
import qualified Data.Graph.Component.Node.Construction      as Term
import qualified Data.Graph.Data                             as Component
import qualified Data.Graph.Data.Graph.Class                 as Graph
import qualified Data.Graph.Data.Layer.Layout                as Layout
import qualified Foreign.Marshal.Alloc                       as Mem
import qualified Foreign.Storable                            as Storable
import qualified Language.Symbol.Operator.Assoc              as Assoc
import qualified Language.Symbol.Operator.Assoc              as Assoc
import qualified Language.Symbol.Operator.Prec               as Prec
import qualified Luna.IR                                     as IR
import qualified Luna.IR.Layer                               as Layer
import qualified Luna.Pass                                   as Pass
import qualified Luna.Pass.Attr                              as Attr
import qualified Luna.Pass.Parsing.ExprBuilder               as ExprBuilder
import qualified Luna.Pass.Parsing.Macro                     as Macro
import qualified Luna.Pass.Parsing.Parser                    as PP
import qualified Luna.Pass.Scheduler                         as Scheduler
import qualified Luna.Syntax.Text.Parser.Data.CodeSpan       as CodeSpan
import qualified Luna.Syntax.Text.Parser.Data.Name.Hardcoded as Hardcoded
import qualified Luna.Syntax.Text.Parser.Data.Name.Special   as Name
import qualified Luna.Syntax.Text.Parser.IR.Ast              as Ast
import qualified Luna.Syntax.Text.Parser.IR.Ast              as Parsing (Parser, SyntaxVersion (..))
import qualified Luna.Syntax.Text.Parser.IR.Term             as Parsing
import qualified Luna.Syntax.Text.Parser.Pass.Definition     as Parser
import qualified Luna.Syntax.Text.Scope                      as Scope
import qualified OCI.Data.Name                               as Name

import Data.Graph.Data.Graph.Class           (Graph)
import Data.Text.Position                    (Delta)
import Data.Text32                           (Text32)
import Luna.Pass                             (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.IR.Term       (Ast)
import OCI.Data.Name                         (Name)
-- import Luna.Syntax.Text.Parser.Pass.Class    (IRBS, Parser)
import Luna.Syntax.Text.Scope  (Scope)
import Luna.Syntax.Text.Source (Source)
import OCI.IR.Link.Class       (type (*-*), Link)
import Test.Hspec              (Arg, Example, Expectation, Spec, describe, it)
import Test.Hspec.Core         (SpecM)

import Luna.IR.Term.Ast.Invalid (adjacentOperators, assocConflict,
                                 emptyExpression, missingRelation,
                                 missingSection, noAssoc, unexpectedSuffix)


import qualified Luna.Pass.Parsing.Parser as P

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

e :: Text32 -> Ast.SimpleAst -> IO ()
e src out = sast `shouldBe` out where
    sast = Ast.simplify ast
    ast  = flip PP.runWith src $ do
        let rplus  = ">>+" :: Name
            noplus = ">+<" :: Name
        Assoc.write Assoc.Right rplus
        Assoc.write Assoc.None  noplus
        Prec.writeRel EQ ("+" :: Name) rplus
        Prec.writeRel EQ ("+" :: Name) noplus

        Macro.expr

e' :: Text32 -> IO ()
e' src = e src (convertVia @String src)


----------------------
-- === Literals === --
----------------------

__ :: Convertible' Ast.SimpleAst a => a
__ = convert' Ast.SMissing

identSpec :: Spec
identSpec = describe "identifier" $ do

  describe "valid" $ do
    "var"             $ e' "var"
    "_var"            $ e' "_var"
    "var'"            $ e' "var'"
    "var''"           $ e' "var''"
    "unicode"         $ e' "фываΧξωβ김동욱"
    "Cons"            $ e' "Cons"
    "Cons'"           $ e' "Cons'"
    "Cons''"          $ e' "Cons''"

  describe "invalid" $ do
    "var'o"           $ e "var'o"         $ unexpectedSuffix 1
    "var_a"           $ e "var_a"         $ unexpectedSuffix 2
    "Cons'o"          $ e "Cons'o"        $ unexpectedSuffix 1
    "Cons_a"          $ e "Cons_a"        $ unexpectedSuffix 2
    "var⸗"            $ e "var⸗"          $ unexpectedSuffix 1
    "Cons⸗"           $ e "Cons⸗"         $ unexpectedSuffix 1

literalNumberSpec :: Spec
literalNumberSpec = describe "number" $ do
    let biggerThanInt64   = convert (show (maxBound :: Int64)) <> "0"
    it "positive"     $ e' "1"
    it "negative"     $ e  "a -1"          $ "a" (Name.uminus __ 1)
    it "zero pxd"     $ e' "01"
    it "real"         $ e  "1.23"          $ "." 1 23
    it "spaced"       $ e  "123 456"       $ 123 456
    it "spaced reals" $ e  "123 456 . 789" $ "." (123 456) 789
    it "int > 64 bit" $ e' biggerThanInt64
    it "invalid sfx"  $ e "1a"             $ unexpectedSuffix 1


operatorSpec :: Spec
operatorSpec = describe "operator" $ do

  describe "simple" $ do
    "single"          $ e "a - b"         $ "a" - "b"
    "modifier"        $ e "a += 1"        $ "+=" "a" 1
    "application"     $ e "a b"           $ "a" "b"

  describe "section" $ do
    "line left"       $ e "+ a"           $ __ + "a"
    "line right"      $ e "a +"           $ "a" + __
    "glued left"      $ e "+a"            $ __ + "a"
    "glued right"     $ e "a+"            $ "a" + __
    "glued left app"  $ e "a +b * c"      $ "a" (__ + "b") * "c"
    "glued right app" $ e "a b+ * c"      $ "a" ("b" + __) * "c"

  describe "precedence" $ do
    "simple"          $ e "a + b * c"     $ "a" + ("b" * "c")
    "spaced"          $ e "a+b * c"       $ ("a" + "b") * "c"

  describe "multiline" $ do
    "grouping"        $ e "a\n b c"       $ "a" ("b" "c")
    "section 1"       $ e "a +\n b c"     $ ("a" + __) ("b" "c")
    "section 2"       $ e "a+ \n b c"     $ ("a" + __) ("b" "c")
    "section 3"       $ e "a \n +b c"     $ "a" ((__ + "b") "c")
    "continuation"    $ e "a \n + b c"    $ "a" + "b" "c"

  describe "invalid" $ do
    "adj infix"       $ e "a + + b"       $ adjacentOperators "a" "b"
    "adj infix 2"     $ e "a + + + b"     $ adjacentOperators "a" "b"
    "adj postfix"     $ e "a + +"         $ adjacentOperators "a" __
    "adj prefix"      $ e "+ + a"         $ adjacentOperators __ "a"
    "wrong assoc"     $ e "a + b >>+ c"   $ assocConflict   ("a" + "b") "c"
    "no prec"         $ e "a + b +++ c"   $ missingRelation ("a" + "b") "c"
    "no assoc"        $ e "a >+< b >+< c" $ noAssoc (">+<" "a" "b") "c"



    "a)"                    $ e "a)"         $ ")" "a" __
    "group"                 $ e "(a b)"      $ "(_)" ("a" "b")


shuffle :: [a] -> [a] -> [a]
shuffle (a:as) (b:bs) = a:b:shuffle as bs
shuffle _ _           = []


-- | Testing all possible missing sections scenarios. We've got in scope
--   definitions of both `if_then_else` as well as `if_then`. We try here all
--   possible combinations of correct and incorrect applications, like
--   `if a then else c` or `if then else c`.
missingSectionsSpec :: Spec
missingSectionsSpec = describe "mixfix" $ let

    tst :: String -> [String] -> [Either Ast.SimpleAst String] -> SpecM () ()
    tst n ns as = it_e (mconcat $ shuffle ns (pat <$> as))
                $ Ast.appMany (convert n) (exp <$> as)

    opt   :: String -> [Either Ast.SimpleAst String]
    opt   = \a -> [Right a, Left emptyExpression]
    noSec = Left missingSection
    pat   = either (const " ") (\t -> " " <> t <> " ")
    exp   = either id convert

    _ite a b c = tst "if_then_else" ["if", "then", "else"] [a,b,c]
    _it  a b   = tst "if_then"      ["if", "then"        ] [a,b]
    _ie  a   c = tst "if_then_else" ["if", "", "else"    ] [a, noSec, c]
    _i   a     = tst "if_then"      ["if", ""            ] [a, noSec]

    in do
        sequence_ $ _ite <$> opt "a" <*> opt "b" <*> opt "c"
        sequence_ $ _it  <$> opt "a" <*> opt "b"
        sequence_ $ _ie  <$> opt "a" <*> opt "c"
        sequence_ $ _i   <$> opt "a"



it_e :: String -> Ast.SimpleAst -> SpecM () ()
it_e s t = it s (e (convert s) t)

funcDefSpec :: Spec
funcDefSpec = describe "function" $ do
--     it "def"        $ exprAs "def" "(InvalidFunctionDefinition)"
--     it "def _"      $ exprAs "def _" "def (InvalidFunctionName): (MissingColonBlock)"
--     it "def f"      $ exprAs "def f" "def f: (MissingColonBlock)"
--     it "def f +: a" $ exprAs "def f +: a" "def f (InvalidFunctionArg): a"
--     it "def f a:"   $ expr   "def f a:"
    it "def f a: a" $ e   "def f a b c: a" $ "def_:" "f" ["a", "b", "c"] "a"
    it "def f a: a" $ e   "def f a" $ "def_:"


-- it :: (HasCallStack, Example a) => String -> a -> SpecWith (Arg a)





-- literalStringSpec :: Spec
-- literalStringSpec = describe "string" $ do
--     describe "raw" $ do
--         it "empty"             $ expr   [s|""|]
--         it "simple"            $ expr   [s|"test sentence"|]
--         it "tripple d-quoted"  $ exprAs [s|"""Test"""|] [s|"Test"|]
--         it "escape quote"      $ expr   [s|"foo\""|]
--         it "escape escape"     $ expr   [s|"foo\\"|]
--         it "implicite escape"  $ exprAs [s|"foo\bar"|] [s|"foo\\bar"|]

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


-- literalListSpec :: Spec
-- literalListSpec = describe "list" $ do
--     it "empty list"              $ expr "[]"           -- [(0,2)]
--     it "singleton list"          $ expr "[a]"          -- [(1,1),(0,3)]
--     it "few elems list"          $ expr "[a, b, c]"    -- [(1,1),(2,1),(2,1),(0,9)]
--     it "list section"            $ expr "[, ]"         -- [(1,0),(1,0),(0,3)]
--     -- it "list with tuple section" $ expr "[(, )]"    -- [(1,4),(0,6)]
--     it "nested lists"            $ expr "[a, [b, c]]"  -- [(1,1),(1,1),(2,1),(2,6),(0,11)]
--     it "list sections"           $ expr "[, a, , b, ]" -- [(1,0),(2,1),(2,0),(2,1),(2,0),(0,12)]
--     it "nested section list"     $ expr "[[, ]]"       -- [(1,4),(0,6)]

-- literalTupleSpec :: Spec
-- literalTupleSpec = describe "tuple" $ do
--     it "3 elems tuple" $ expr "(a, 30, \"ala\")" -- [(1,1),(2,2),(2,5),(0,14)]
--     it "section tuple" $ expr "(, 30)"           -- [(1,0),(2,2),(0,6)]

literalSpec :: Spec
literalSpec = describe "literal" $ do
    literalNumberSpec
--     literalStringSpec
--     literalListSpec
--     literalTupleSpec


-- -------------------
-- -- === Terms === --
-- -------------------

-- termApplicationSpec :: Spec
-- termApplicationSpec = describe "applications" $ do
--     it "single arg application"   $ expr "foo a"      --[(0,3),(1,1),(0,5)]
--     it "multiple arg application" $ expr "foo a b c"  --[(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(0,9)]
--     it "applicated parensed expr" $ expr "foo (a b)"  --[(0,3),(0,1),(1,1),(1,3),(1,5),(0,9)]

-- termTypeSpec :: Spec
-- termTypeSpec = describe "type" $ do
--     it "simple var type"       $ expr   "a :: t"                                -- [(0,1),(4,1),(0,6)]
--     it "typed Vector cons"     $ expr   "Vector 1 2 3 :: Vector Int"            -- [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(0,12),(0,6),(1,3),(4,10),(0,26)]
--     it "typed local variables" $ exprAs "foo a::A b::B" "foo (a :: A) (b :: B)" -- [(0,3),(0,1),(2,1),(1,4),(0,8),(0,1),(2,1),(1,4),(0,13)]
--     it "kind typed expression" $ expr   "pi :: Real :: Type"                    -- [(0,2),(0,4),(4,4),(4,12),(0,18)]
--     -- TODO: op prec

-- termAccessorSpec :: Spec
-- termAccessorSpec = describe "accessor" $ do
--     it "single accessors"                      $ expr   "foo . bar"                       -- [(0,3),(0,9)]
--     it "chained accessors"                     $ expr   "foo . bar . baz"                 -- [(0,3),(0,9),(0,15)]
--     it "delayed accessor (single)"             $ expr   ".foo"                            -- [(0,4)]
--     it "delayed accessor (double)"             $ expr   ".foo.bar"                        -- [(0,8)]
--     it "delayed accessor (tripple)"            $ expr   ".foo.bar.baz"                    -- [(0,12)]
--     it "delayed accessor (operator)"           $ expr   ".+"                              -- [(0,2)]
--     it "delayed accessor (nested op)"          $ expr   ".foo.+"                          -- [(0,6)]
--     it "delayed accessor as argument"          $ expr   "foo .pos"                        -- [(0,3),(1,4),(0,8)]
--     it "running method of delayed accessor"    $ expr   ".foo . bar"                      -- [(0,4),(0,10)]
--     it "parensed delayed accessor"             $ expr   "(.foo)"                          -- [(1,4),(0,6)]
--     it "parensed delayed accessor as argument" $ expr   "test (.foo)"                     -- [(0,4),(1,4),(1,6),(0,11)]
--     it "operator unspaced accessors"           $ exprAs "2.+ 1" "2 . + 1"                 -- [(0,1),(0,3),(1,1),(0,5)]
--     it "complex spaced accessors 1"            $ expr   "foo 1 . bar 2 3 . baz 4 5"       -- [(0,3),(1,1),(0,5),(0,11),(1,1),(0,13),(1,1),(0,15),(0,21),(1,1),(0,23),(1,1),(0,25)]
--     it "complex spaced accessors 2"            $ exprAs "foo 1 . bar 2.baz 3.ban"
--                                                         "foo 1 . bar (2 . baz) (3 . ban)" -- [(0,3),(1,1),(0,5),(0,11),(0,1),(1,5),(0,17),(0,1),(1,5),(0,23)]
-- termInfixSpec :: Spec
-- termInfixSpec = describe "infixe" $ do
--     it "infix operator on variables"         $ expr   "a + b"                   -- [(0,1),(1,1),(0,3),(1,1),(0,5)]
--     it "infix operator on applications"      $ expr   "a b + c d"               -- [(0,1),(1,1),(0,3),(1,1),(0,5),(0,1),(1,1),(1,3),(0,9)]
--     it "infixl operators"                    $ expr   "a - b - c"               -- [(0,1),(1,1),(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(0,9)]
--     it "infixl operators with spaced prec 1" $ exprAs "a - b-c" "a - (b - c)"   -- [(0,1),(1,1),(0,3),(0,1),(0,1),(0,2),(0,1),(1,3),(0,7)]
--     it "infixl operators with spaced prec 2" $ exprAs "a-b - c" "a - b - c"     -- [(0,1),(0,1),(0,2),(0,1),(0,3),(1,1),(0,5),(1,1),(0,7)]
--     it "infixl operators with mixed prec 1"  $ expr   "a + b * c"               -- [(0,1),(1,1),(0,3),(0,1),(1,1),(0,3),(1,1),(1,5),(0,9)]
--     it "infixl operators with mixed prec 2"  $ expr   "a * b + c"               -- [(0,1),(1,1),(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(0,9)]
--     it "infixl operators with mixed prec 3"  $ exprAs "a * b+c" "a * (b + c)"   -- [(0,1),(1,1),(0,3),(0,1),(0,1),(0,2),(0,1),(1,3),(0,7)]
--     it "infixl operators with mixed prec 4"  $ exprAs "a*b + c" "a * b + c"     -- [(0,1),(0,1),(0,2),(0,1),(0,3),(1,1),(0,5),(1,1),(0,7)]
--     it "space-based precedence"              $ exprAs "a - b-c" "a - (b - c)"   -- [(0,1),(1,1),(0,3),(0,1),(0,1),(0,2),(0,1),(1,3),(0,7)]
--     it "$-separated sub-expressions"         $ expr   "foo $ bar baz"           -- [(0,3),(1,1),(0,5),(0,3),(1,3),(1,7),(0,13)]

-- termUnarySpec :: Spec
-- termUnarySpec = describe "unary minus" $ do
--     it "unary minus as expression"       $ expr   "-a"          --[(0,1),(0,1),(0,2)]
--     it "unary minus in application"      $ expr   "foo -a"      --[(0,3),(0,1),(0,1),(1,2),(0,6)]
--     it "app with multiple unary minuses" $ expr   "foo -a 4 -5" --[(0,3),(0,1),(0,1),(1,2),(0,6),(1,1),(0,8),(0,1),(0,1),(1,2),(0,11)]
--     it "binary minus expression"         $ expr   "foo - a"     --[(0,3),(1,1),(0,5),(1,1),(0,7)]
--     it "right minus section"             $ exprAs "a-" "(a -)"  --[(0,1),(0,1),(0,2)]

-- termSectionSpec :: Spec
-- termSectionSpec = describe "section" $ do
--     it "operator as expression"           $ expr   "+"                                      -- [(0,1)]
--     it "border unspaced section (l)"      $ exprAs "+a"            "(+ a)"                  -- [(0,1),(0,1),(0,2)]
--     it "border unspaced section (r)"      $ exprAs "a+"            "(a +)"                  -- [(0,1),(0,1),(0,2)]
--     it "border unspaced section (l/r)"    $ exprAs "+a+"           "((+ a) +)"              -- [(0,1),(0,1),(0,2),(0,1),(0,3)]
--     it "border spaced section (l)"        $ exprAs "+ a"           "(+ a)"                  -- [(0,1),(1,1),(0,3)]
--     it "border spaced section (r)"        $ exprAs "a +"           "(a +)"                  -- [(0,1),(1,1),(0,3)]
--     it "border spaced section (l/r)"      $ exprAs "+ a +"         "((+ a) +)"              -- [(0,1),(1,1),(0,3),(1,1),(0,5)]
--     it "border section application (l)"   $ exprAs "+ foo a"       "(+ foo a)"              -- [(0,1),(0,3),(1,1),(1,5),(0,7)]
--     it "border section application (r)"   $ exprAs "foo a +"       "(foo a +)"              -- [(0,3),(1,1),(0,5),(1,1),(0,7)]
--     it "border section application (l/r)" $ exprAs "+ foo a +"     "((+ foo a) +)"          -- [(0,1),(0,3),(1,1),(1,5),(0,7),(1,1),(0,9)]
--     it "section l application (l)"        $ exprAs "foo +a*2"      "foo (+ a * 2)"          -- [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(1,4),(0,8)]
--     it "section l application (r)"        $ exprAs "foo a*2+"      "foo (a * 2 +)"          -- [(0,3),(0,1),(0,1),(0,2),(0,1),(0,3),(0,1),(1,4),(0,8)]
--     it "section l application (l/r)"      $ exprAs "foo +a*2+"     "foo ((+ a * 2) +)"      -- [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(0,1),(1,5),(0,9)]
--     it "section r application (l)"        $ exprAs "+a*2 foo"      "(+ a * 2) foo"          -- [(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(1,3),(0,8)]
--     it "section r application (r)"        $ exprAs "a*2+ foo"      "(a * 2 +) foo"          -- [(0,1),(0,1),(0,2),(0,1),(0,3),(0,1),(0,4),(1,3),(0,8)]
--     it "section r application (l/r)"      $ exprAs "+a*2+ foo"     "((+ a * 2) +) foo"      -- [(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(0,1),(0,5),(1,3),(0,9)]
--     it "section l/r application (l)"      $ exprAs "foo +a*2 bar"  "foo (+ a * 2) bar"      -- [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(1,4),(0,8),(1,3),(0,12)]
--     it "section l/r application (r)"      $ exprAs "foo a*2+ bar"  "foo (a * 2 +) bar"      -- [(0,3),(0,1),(0,1),(0,2),(0,1),(0,3),(0,1),(1,4),(0,8),(1,3),(0,12)]
--     it "section l/r application (l/r)"    $ exprAs "foo +a*2+ bar" "foo ((+ a * 2) +) bar"  -- [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(0,1),(1,5),(0,9),(1,3),(0,13)]

-- termGroupSpec :: Spec
-- termGroupSpec = describe "group" $ do
--     it "group as expression"             $ expr   "(foo)"                                -- [(1,3),(0,5)]
--     it "group as unspaced left section"  $ exprAs "(+a)"               "((+ a))"         -- [(0,1),(0,1),(1,2),(0,4)]
--     it "group as unspaced right section" $ exprAs "(a+)"               "((a +))"         -- [(0,1),(0,1),(1,2),(0,4)]
--     it "group as unspaced l/r section"   $ exprAs "(+a+)"              "(((+ a) +))"     -- [(0,1),(0,1),(0,2),(0,1),(1,3),(0,5)]
--     it "group as spaced left section"    $ exprAs "(+ a)"              "((+ a))"         -- [(0,1),(1,1),(1,3),(0,5)]
--     it "group as spaced right section"   $ exprAs "(a +)"              "((a +))"         -- [(0,1),(1,1),(1,3),(0,5)]
--     it "group as spaced l/r section"     $ exprAs "(+ a +)"            "(((+ a) +))"     -- [(0,1),(1,1),(0,3),(1,1),(1,5),(0,7)]
--     it "group with left section"         $ exprAs "(+ foo a)"          "((+ foo a))"     -- [(0,1),(0,3),(1,1),(1,5),(1,7),(0,9)]
--     it "group with right section"        $ exprAs "(foo a +)"          "((foo a +))"     -- [(0,3),(1,1),(0,5),(1,1),(1,7),(0,9)]
--     it "group with l/r section"          $ exprAs "(+ foo a +)"        "(((+ foo a) +))" -- [(0,1),(0,3),(1,1),(1,5),(0,7),(1,1),(1,9),(0,11)]
--     it "group with complex expression"   $ expr   "(foo . bar 2 + 11)"                   -- [(0,3),(0,9),(1,1),(0,11),(1,1),(0,13),(1,2),(1,16),(0,18)]
--     it "group application 1"             $ exprAs "(foo a)(b)"         "(foo a) (b)"     -- [(0,3),(1,1),(1,5),(0,7),(1,1),(0,3),(0,10)]
--     it "group application 2"             $ expr   "(foo a) b (c)"                        -- [(0,3),(1,1),(1,5),(0,7),(1,1),(0,9),(1,1),(1,3),(0,13)]
--     it "group application 3"             $ expr   "test (foo a) b (c)"                   -- [(0,4),(0,3),(1,1),(1,5),(1,7),(0,12),(1,1),(0,14),(1,1),(1,3),(0,18)]
--     it "wildcard group application"      $ expr   "(foo _) a"                            -- [(0,3),(1,1),(1,5),(0,7),(1,1),(0,9)]

-- termPatternSpec :: Spec
-- termPatternSpec = describe "pattern" $ do
--     it "variable"                  $ expr   "a = val"                                   -- [(0,1),(3,3),(0,7)]
--     it "wildcard"                  $ expr   "_ = val"                                   -- [(0,1),(3,3),(0,7)]
--     it "deconstructor (no args)"   $ expr   "Vector = val"                              -- [(0,6),(3,3),(0,12)]
--     it "deconstructor (with args)" $ expr   "Vector x y z = val"                        -- [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(0,12),(3,3),(0,18)]
--     it "grouped"                   $ expr   "(Vector x y z) = val"                      -- [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(1,12),(0,14),(3,3),(0,20)]
--     it "module-qualified"          $ exprAs "A.B.C x y z = val" "A . B . C x y z = val" -- [(0,1),(0,3),(0,5),(1,1),(0,7),(1,1),(0,9),(1,1),(0,11),(3,3),(0,17)]

-- -- FIXME: Hack, for details see Parsing.hs
-- termModifierSpec :: Spec
-- termModifierSpec = describe "modifiers" $ do
--     it "field update"            $ expr "a = a.x = v"  -- [(0,1),(0,1),(0,3),(3,1),(3,7),(0,11)]
--     it "field drop update"       $ expr "a.x = v"      -- [(0,1),(0,3),(3,1),(0,7)]
--     it "field modification"      $ expr "a = a.x += v" -- [(0,1),(0,1),(0,3),(4,1),(3,8),(0,12)]
--     it "field drop modification" $ expr "a.x += v"     -- [(0,1),(0,3),(4,1),(0,8)]
--     -- FIXME: Correct implementation (vvv). Uncomment when above hack will be invalid.
--         -- it "variable's field update"                    $ shouldParseItself' expr "a = a.x = v"                               [(0,1),(0,1),(5,1),(3,7),(0,11)]
--         -- it "variable's field drop update"               $ shouldParseItself' expr "a.x = v"                                   [(0,1),(5,1),(0,7)]
--         -- it "variable's field modification"              $ shouldParseItself' expr "a = a.x += v"                              [(0,1),(0,1),(6,1),(3,8),(0,12)]
--         -- it "variable's field drop modification"         $ shouldParseItself' expr "a.x += v"                                  [(0,1),(6,1),(0,8)]

-- termLambdaSpec :: Spec
-- termLambdaSpec = describe "lambdas" $ do
--     it "identity (not spaced)"       $ exprAs "x:x"  "x: x"               -- [(0,1),(1,1),(0,3)]
--     it "identity (l   spaced)"       $ exprAs "x :x" "x: x"               -- [(0,1),(2,1),(0,4)]
--     it "identity (r   spaced)"       $ expr   "x: x"                      -- [(0,1),(2,1),(0,4)]
--     it "identity (l/r spaced)"       $ exprAs "x : x" "x: x"              -- [(0,1),(3,1),(0,5)]
--     it "double lambda"               $ expr   "a: b: a + b"               -- [(0,1),(0,1),(0,1),(1,1),(0,3),(1,1),(2,5),(2,8),(0,11)]
--     it "complex pattern lambda"      $ expr   "(Vector x y z): x + y + z" -- [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(1,12),(0,14),(0,1),(1,1),(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(2,9),(0,25)]
--     it "wildcard arg lambda"         $ expr   "_: 1"                      -- [(0,1),(2,1),(0,4)]
--     it "double wildcard lambda"      $ expr   "_: _"                      -- [(0,1),(2,1),(0,4)]
--     it "lambda as parensed argument" $ expr   "foo (x: x + 1)"            -- [(0,3),(0,1),(0,1),(1,1),(0,3),(1,1),(2,5),(1,8),(1,10),(0,14)]
--     it "lambda as $-argument"        $ expr   "foo $ x: x + 1"            -- [(0,3),(1,1),(0,5),(0,1),(0,1),(1,1),(0,3),(1,1),(2,5),(1,8),(0,14)]

-- termSpec :: Spec
-- termSpec = describe "term" $ do
--     termApplicationSpec
--     termTypeSpec
--     termAccessorSpec
--     termInfixSpec
--     termUnarySpec
--     termSectionSpec
--     termGroupSpec
--     termPatternSpec
--     termModifierSpec
--     termLambdaSpec


-- ------------------
-- -- === Unit === --
-- ------------------

-- definitionFunctionSpec :: Spec
-- definitionFunctionSpec = describe "function" $ do
--     it "no argument function definition" $ expr "def foo: bar"       -- [(4,3),(2,3),(0,12)]
--     it "simple function definition"      $ expr "def foo a b: a + b" -- [(4,3),(1,1),(1,1),(0,1),(1,1),(0,3),(1,1),(2,5),(0,18)]

-- unitSpec :: Spec
-- unitSpec = describe "unit definitions" $ do
--     it "value definition"               $ unit_n "def pi: 3.14"       -- [(4,2),(0,12),(0,12),(0,12)]
--     it "expression definition"          $ unit_n "def foo: a + b"     -- [(4,3),(0,14),(0,14),(0,14)]
--     it "function definition"            $ unit_n "def foo a b: a + b" -- [(4,3),(0,18),(0,18),(0,18)]
--     it "operator definition"            $ unit_n "def + a b: a . + b" -- [(4,1),(0,16),(0,16),(0,16)]
--     it "function signature definition"  $ unit_n "def foo :: a -> Vector a"              -- [(4,3),(0,1),(1,2),(0,4),(0,6),(1,1),(1,8),(4,13),(0,24),(0,24),(0,24)]

-- invalidUnitSpec :: Spec
-- invalidUnitSpec = describe "invalid unit definitions" $ do
--     it "orphan def"
--         $ unitAs_n "def" "def Invalid FunctionHeader: Invalid FunctionBlock"
--     it "def without body 1"
--         $ unitAs_n "def foo" "def foo: Invalid FunctionBlock"
--     it "def without body 2"
--         $ unitAs_n "def foo:" "def foo:"


-- caseSpec :: Spec
-- caseSpec = describe "case expression" $ do
--     it "simple case expression"    $ expr "case v of\n    A a: b" -- [(5,1),(0,1),(1,1),(0,3),(2,1),(8,6),(0,20)]
--     it "multiline case expression" $ expr ("case v of"
--                                        </> "    A: a"
--                                        </> "    B: b"
--                                           ) -- [(5,1),(0,1),(2,1),(8,4),(0,1),(2,1),(5,4),(0,27)]

-- layoutSpec :: Spec
-- layoutSpec = describe "layout" $ do
--     it "nested lambda layout" $ expr ("def main:"
--                                   </> "    x = a: b:"
--                                   </> "        a + b"
--                                   </> "        a - b"
--                                   </> "    c"
--                                   )

--     it "nested lambda layout" $ unitAs   ("import Std.Base"
--                                       </> "def foo:"
--                                       </> "«0»def main:"
--                                       </> "    «1»4"
--                                       )  ("imports ..."
--                                       </> "def foo:"
--                                       </> "«0»def main: «1»4"
--                                       )

-- -- import Std.Base
-- -- def foof:
-- -- «0»def main:
-- --     «1»4

-- definitionSpec :: Spec
-- definitionSpec = do
--     definitionFunctionSpec
--     unitSpec
--     invalidUnitSpec
--     caseSpec

-- «12»




-- fixSpec :: Spec
-- fixSpec = describe "error" $ it "x" $ do
--     pure () :: IO ()
--     putStrLn "\n"

--     -- pprint $ Parser.runParserxx__ Parsing.expr "a . b -> c -= d >= f "
--     -- pprint $ Parser.runParserxx__ Parsing.expr "a -> b -> a + b"
--     -- pprint $ Parser.runParserxx__ Parsing.Syntax1 Parsing.expr "def foo a:\n a"
--     -- pprint $ Parser.runParserxx__ Parsing.Syntax2 Parsing.expr "foo (bar baz"
--     pprint $ Parser.run Parsing.Syntax2 [s|
-- foo = a + b

-- |]



fixSpec :: Spec
fixSpec = describe "error" $ it "x" $ do
    pure () :: IO ()
    putStrLn "\n"

    -- pprint $ Parser.runParserxx__ Parsing.expr "a . b -> c -= d >= f "
    -- pprint $ Parser.runParserxx__ Parsing.expr "a -> b -> a + b"
    -- pprint $ Parser.runParserxx__ Parsing.Syntax1 Parsing.expr "def foo a:\n a"
    -- pprint $ Parser.runParserxx__ Parsing.Syntax2 Parsing.expr "foo (bar baz"
    let input     = "if else"
        toks      = Parser.run Parsing.Syntax1 input
        layouted  = ExprBuilder.discoverLayouts toks
        statement = ExprBuilder.buildFlatStatement layouted
        stream    = ExprBuilder.buildStream toks

    putStrLn "\nTOKS:\n"
    pprint toks

    putStrLn "\nLAYOUTED:\n"
    pprint layouted

    putStrLn "\nSTATEMENT:\n"
    pprint statement

    putStrLn "\nSTREAM:\n"
    pprint stream


    putStrLn "\nRESULT:\n"
    pprint $ Ast.simplify $ PP.run2 input


    -- print $ foo == Ast.simplify ast
    -- putStrLn "\nSUB STREAMS:\n"
    -- pprint sstream

    -- putStrLn "\nEXPR STREAM:\n"
    -- pprint estream

    -- putStrLn "\nSTREAM:\n"
    -- pprint stream
    -- -- pprint $ ExprBuilder.subStreams toks

    -- e' <- State.evalDefT @Scope.Scope $ do
    --     Hardcoded.hardcodePrecRelMap
    --     Assoc.write Assoc.Right ("-" :: Name)

    --     ExprBuilder.buildExpr stream

    -- putStrLn "\nEXPR:\n"
    -- pprint e'
    -- putStrLn "\n=========\n"
    -- printCodePure e'


    True `shouldBe` False

-- type Main

--     test : Int -> Int -> [Char]
--     test = a -> b -> a + b . show . convert

-- type Internal
--     foo

-- |]
    -- runParser Parsing.Syntax1 "foo = \n a + b\nbar = c + d"
    -- True `shouldBe` False
    -- it "error" $ expr "def foo:\n x = 1\n def"

spec :: Spec
spec = do
    identSpec
    literalSpec
    operatorSpec
    missingSectionsSpec
    -- funcDefSpec
    -- fixSpec
    -- termSpec
    -- definitionSpec
    -- fixSpec
    -- layoutSpec
    -- pure (s)


-- module Luna.Test.Source.Text.ParserSpec where

-- import           Luna.Prelude        hiding (String)
-- import qualified Luna.Prelude        as P

-- import           Control.Exception   (PatternMatchFail)
-- import           Data.Maybe          (isJust)
-- import qualified OCI.Pass           as Pass
-- import           OCI.Pass.Definition.Declaration
-- import           OCI.Pass           (Pass, SubPass)
-- import           OCI.Pass.Manager   (MonadPassManager)
-- import           Luna.IR           hiding (IRBuilder, expr, unit')
-- import qualified OCI.IR.Repr.Vis    as Vis
-- import Data.Int (Int64)

-- import Test.Hspec.Megaparsec hiding (shouldParse)
-- import Test.Hspec

-- import Luna.Syntax.Text.Parser.IR.Term
-- import Text.Megaparsec (eof, ParseError)
-- import qualified Luna.Syntax.Text.Pretty.Pretty as CodeGen
-- import Luna.Syntax.Text.Parser.IR.Class (Error, IRB(IRB), Parsing)

-- import System.Log (Logging, dropLogs)
-- import qualified Luna.Syntax.Text.Layer.Loc as Loc
-- import qualified Luna.Syntax.Text.Parser.IR.Class   as Parser
-- import qualified Luna.Syntax.Text.Parser.IR.Term  as Parsing
-- import           Luna.Syntax.Text.Parser.IR.Class   (IRParser, ParsedExpr, IRBSParser)
-- import qualified Luna.Syntax.Text.Parser.Data.CodeSpan as CodeSpan
-- import           Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
-- import           Luna.Syntax.Text.Parser.Errors   (Invalids)
-- import           Control.Monad.Raise (Throws, tryAll)
-- import           Data.TypeDesc (getTypeDesc)
-- import Data.Text.Position
-- import Control.Monad.State.Dependent
-- import Luna.Syntax.Text.Parser.State.Marker (MarkedExprMap)
-- import Luna.Syntax.Text.Source

-- import qualified Luna.Syntax.Text.Lexer as Lexer
-- import Data.Text32

-- --

-- data ParsingTest
-- type instance Abstract ParsingTest = ParsingTest
-- type instance Inputs  Net   ParsingTest = '[AnyExpr, AnyExprLink]
-- type instance Outputs Net   ParsingTest = '[AnyExpr, AnyExprLink]
-- type instance Inputs  Layer ParsingTest = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
-- type instance Outputs Layer ParsingTest = '[AnyExpr // CodeSpan]
-- type instance Inputs  Attr  ParsingTest = '[Source, ParsedExpr]
-- type instance Outputs Attr  ParsingTest = '[ParsedExpr, MarkedExprMap]
-- type instance Inputs  Event ParsingTest = '[] -- will never be used
-- type instance Outputs Event ParsingTest = '[New // AnyExpr, New // AnyExprLink]
-- type instance Preserves     ParsingTest = '[]

-- testParsing_raw :: (MonadIO m, MonadFix m, PrimMonad m) => IRBSParser SomeExpr -> P.String -> m (Either SomeException (P.String, [(Int, Int)]))
-- testParsing_raw p str = tryAll $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
--     runRegs' False

--     Loc.init
--     attachLayer 5 (getTypeDesc @Range) (getTypeDesc @AnyExpr)

--     CodeSpan.init
--     attachLayer 5 (getTypeDesc @CodeSpan) (getTypeDesc @AnyExpr)

--     setAttr (getTypeDesc @MarkedExprMap) $ (mempty :: MarkedExprMap)
--     setAttr (getTypeDesc @ParsedExpr)    $ (error "Data not provided: ParsedExpr")
--     setAttr (getTypeDesc @Invalids)      $ (mempty :: Invalids)

--     setAttr (getTypeDesc @Source) $ (convert str :: Source)
--     Pass.eval' @Parsing     $ Parsing.parsingPassM p
--     spans  <- Pass.eval' @ParsingTest $ do
--         es <- exprs
--         ls <- getLayer @CodeSpan <$>= es
--         return $ (convert . view CodeSpan.realSpan) <$> ls
--     code <- Pass.eval' @ParsingTest $ CodeGen.subpass CodeGen.SimpleStyle . unsafeGeneralize . unwrap =<< getAttr @ParsedExpr
--     return (convert code, convert (spans :: [(Delta, Delta)]))

-- testParsing :: (MonadIO m, MonadFix m, PrimMonad m) => IRBSParser SomeExpr -> P.String -> m (Either SomeException (P.String, [(Int, Int)]))
-- testParsing = fmap3 dropNulls .: testParsing_raw

-- dropNulls :: [(Int,Int)] -> [(Int,Int)]
-- dropNulls = filter (/= (0,0))

-- shouldParseAs p s out = do
--     r <- mapLeft displayException . fmap fst <$> testParsing p s
--     shouldBe r (Right out)

-- shouldParseItself p s = shouldParseAs p s s


-- shouldParseAs' p s out spans = do
--     r <- mapLeft displayException <$> testParsing p s
--     shouldBe r (Right (out, spans))

-- shouldParseAs_raw' p s out spans = do
--     r <- mapLeft displayException <$> testParsing_raw p s
--     shouldBe r (Right (out, spans))

-- shouldParseAsx p s spans = do
--     r <- mapLeft displayException . fmap snd <$> testParsing p s
--     shouldBe r (Right spans)

-- shouldParseItself' p s = shouldParseAs' p s s
-- shouldParseItself_raw' p s = shouldParseAs_raw' p s s
-- shouldParseItself'' p s = shouldParseAs' p s ('\n':s)
-- shouldParseAs''     p s s' = shouldParseAs' p s ('\n':s')
-- shouldParse          p s = shouldParseAsx p s

l </> r = l <> "\n" <> r

-- spec :: Spec
-- spec = do











--         describe "imports" $ do
--             it "import everything"                          $ shouldParseItself' unit' "import Std.Math"                         [(7,8),(0,15),(0,15),(0,15)]
--             it "import selected"                            $ shouldParseItself' unit' "import Std.Math: Vector Scalar"          [(7,8),(0,30),(0,30),(0,30)]
--             it "import relative"                            $ shouldParseItself' unit' "import .Local"                           [(7,6),(0,13),(0,13),(0,13)]
--             it "import from World"                          $ shouldParseAs'     unit' "import World: Std" "import #World#: Std" [(7,5),(0,17),(0,17),(0,17)]

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
--                                                                                         ) [(13,1),(0,1),(2,1),(0,1),(1,1),(0,1),(1,1),(6,7),(7,1),(3,8),(5,11),(4,3),(5,10),(0,58),(0,58),(0,58)]
--         describe "block layout" $ do
--             -- TODO: prawdziwe bloki
--             let lam1  = "lam: foo"
--                     </> "     bar"
--                 lam1' = "lam:"
--                     </> "    foo"
--                     </> "    bar"
--             it "inline block"                               $ shouldParseAs' expr lam1 lam1' [(0,3),(0,3),(6,3),(2,12),(0,17)]
--             it "newline block"                              $ shouldParseItself' expr lam1'  [(0,3),(0,3),(5,3),(6,11),(0,20)]
--             it "nested blocks"                              $ shouldParseItself' expr ( "a:"
--                                                                                     </> "    foo"
--                                                                                     </> "    bar b:"
--                                                                                     </> "        baz"
--                                                                                     </> "        bax"
--                                                                                     </> "    bam"
--                                                                                         ) [(0,1),(0,3),(0,3),(0,1),(0,3),(9,3),(10,15),(1,26),(5,30),(0,38),(5,3),(6,46),(0,53)]

--             -- TODO: structure updates
--             -- describe "structure updates" $ do
--             --     it "field setting"                 $ shouldParseItself expr "foo.x = 5"
--             --     it "field modification (unspaced)" $ shouldParseItself expr "foo.x += 5"
--             --     it "field modification (spaced)"   $ shouldParseItself expr "foo . x += 5"
--             --     it "field modification (lens)"     $ shouldParseItself expr "vec . at pos += 5"


--         describe "markers" $ do
--             it "marked expression"                       $ shouldParseItself' expr "«0»foo bar" [(0,3),(0,3),(1,3),(0,7),(0,10)]
--             it "marked assignment expression"            $ shouldParseItself' expr "«0»a = foo" [(0,3),(0,1),(3,3),(0,7),(0,10)]

--         describe "mixfixes" $ do
--             it "mixfix (if .. then .. else)"             $ shouldParseItself' expr "if a b then c d else e f" [(0,2),(0,1),(1,1),(1,3),(0,6),(0,1),(1,1),(6,3),(0,15),(0,1),(1,1),(6,3),(0,24)]

--         describe "disabled" $ do
--             it "disabled var expression"                 $ shouldParseItself' expr "##foo"                   [(2,3),(0,5)]
--             it "disabled app expression"                 $ shouldParseItself' expr "##foo bar"               [(0,3),(1,3),(2,7),(0,9)]
--             it "disabled expression with space"          $ shouldParseAs'     expr "## foo bar"
--                                                                                     "##foo bar"
--                                                                                     [(0,3),(1,3),(3,7),(0,10)]
--             it "disabled multiline lambda"               $ shouldParseItself' expr "##a:\n    foo\n    bar" [(0,1),(0,3),(5,3),(6,11),(2,18),(0,20)]

--         describe "documentation" $ do
--             it "single line doc comment"                 $ shouldParseItself' expr ("# doc comment stuff"
--                                                                                     </> "def foo a: a"
--                                                                                     ) [(4,3),(1,1),(2,1),(20,12),(0,32)]
--             it "multi-line doc comment"                  $ shouldParseItself' expr ("# Foo bar baz"
--                                                                                     </> "# quux bam"
--                                                                                     </> "def foo a: a"
--                                                                                     ) [(4,3),(1,1),(2,1),(25,12),(0,37)]
--             it "single-line doc comment above expr"      $ shouldParseItself' expr ("# Doc comment _stuff_"
--                                                                                     </> "a = b + c"
--                                                                                     ) [(0,1),(0,1),(1,1),(0,3),(1,1),(3,5),(22,9),(0,31)]
--             it "multi-line doc comment above expr"       $ shouldParseItself' expr ("# Doc line one"
--                                                                                     </> "# doc line two"
--                                                                                     </> "foo = a . b bar"
--                                                                                     ) [(0,3),(0,1),(0,5),(1,3),(3,9),(30,15),(0,45)]

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

