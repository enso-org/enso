module Luna.Test.Source.Text.ParserSpec where

import           Luna.Prelude        hiding (String)
import qualified Luna.Prelude        as P

import           Control.Exception   (PatternMatchFail)
import           Data.Maybe          (isJust)
import qualified OCI.Pass           as Pass
import           OCI.Pass.Class
import           OCI.Pass           (Pass, SubPass)
import           OCI.Pass.Manager   (MonadPassManager)
import           Luna.IR           hiding (IRBuilder, expr, unit')
import qualified OCI.IR.Repr.Vis    as Vis
import Data.Int (Int64)

import Test.Hspec.Megaparsec hiding (shouldParse)
import Test.Hspec

import Luna.Syntax.Text.Parser.Parsing
import Text.Megaparsec (eof, ParseError)
import qualified Luna.Syntax.Text.Pretty.Pretty as CodeGen
import Luna.Syntax.Text.Parser.Parser (Error, IRB(IRB), Parsing)

import System.Log (Logging, dropLogs)
import qualified Luna.Syntax.Text.Layer.Loc as Loc
import qualified Luna.Syntax.Text.Parser.Parser   as Parser
import qualified Luna.Syntax.Text.Parser.Parsing  as Parsing
import           Luna.Syntax.Text.Parser.Parser   (IRParser, ParsedExpr, AsgParser)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import           Luna.Syntax.Text.Parser.Errors   (Invalids)
import           Control.Monad.Raise (Throws, tryAll)
import           Data.TypeDesc (getTypeDesc)
import Data.Text.Position
import Control.Monad.State.Dependent
import Luna.Syntax.Text.Parser.Marker (MarkedExprMap)
import Luna.Syntax.Text.Source


--

data ParsingTest
type instance Abstract ParsingTest = ParsingTest
type instance Inputs  Net   ParsingTest = '[AnyExpr, AnyExprLink]
type instance Outputs Net   ParsingTest = '[AnyExpr, AnyExprLink]
type instance Inputs  Layer ParsingTest = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
type instance Outputs Layer ParsingTest = '[AnyExpr // CodeSpan]
type instance Inputs  Attr  ParsingTest = '[Source, ParsedExpr]
type instance Outputs Attr  ParsingTest = '[ParsedExpr, MarkedExprMap]
type instance Inputs  Event ParsingTest = '[] -- will never be used
type instance Outputs Event ParsingTest = '[New // AnyExpr, New // AnyExprLink]
type instance Preserves     ParsingTest = '[]

testParsing_raw :: (MonadIO m, MonadFix m, PrimMonad m) => AsgParser SomeExpr -> P.String -> m (Either SomeException (P.String, [(Int, Int)]))
testParsing_raw p str = tryAll $ dropLogs $ evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
    runRegs' False

    Loc.init
    attachLayer 5 (getTypeDesc @Range) (getTypeDesc @AnyExpr)

    CodeSpan.init
    attachLayer 5 (getTypeDesc @CodeSpan) (getTypeDesc @AnyExpr)

    setAttr (getTypeDesc @MarkedExprMap) $ (mempty :: MarkedExprMap)
    setAttr (getTypeDesc @ParsedExpr)    $ (error "Data not provided: ParsedExpr")
    setAttr (getTypeDesc @Invalids)      $ (mempty :: Invalids)

    setAttr (getTypeDesc @Source) $ (convert str :: Source)
    Pass.eval' @Parsing     $ Parsing.parsingPassM p
    spans  <- Pass.eval' @ParsingTest $ do
        es <- exprs
        ls <- getLayer @CodeSpan <$>= es
        return $ (convert . view CodeSpan.realSpan) <$> ls
    code <- Pass.eval' @ParsingTest $ CodeGen.subpass CodeGen.SimpleStyle . unsafeGeneralize . unwrap =<< getAttr @ParsedExpr
    return (convert code, convert (spans :: [(Delta, Delta)]))

testParsing :: (MonadIO m, MonadFix m, PrimMonad m) => AsgParser SomeExpr -> P.String -> m (Either SomeException (P.String, [(Int, Int)]))
testParsing = fmap3 dropNulls .: testParsing_raw

dropNulls :: [(Int,Int)] -> [(Int,Int)]
dropNulls = filter (/= (0,0))

shouldParseAs p s out = do
    r <- mapLeft displayException . fmap fst <$> testParsing p s
    shouldBe r (Right out)

shouldParseItself p s = shouldParseAs p s s


shouldParseAs' p s out spans = do
    r <- mapLeft displayException <$> testParsing p s
    shouldBe r (Right (out, spans))

shouldParseAs_raw' p s out spans = do
    r <- mapLeft displayException <$> testParsing_raw p s
    shouldBe r (Right (out, spans))

shouldParseAsx p s spans = do
    r <- mapLeft displayException . fmap snd <$> testParsing p s
    shouldBe r (Right spans)

shouldParseItself' p s = shouldParseAs' p s s
shouldParseItself_raw' p s = shouldParseAs_raw' p s s
shouldParseItself'' p s = shouldParseAs' p s ('\n':s)
shouldParseAs''     p s s' = shouldParseAs' p s ('\n':s')
shouldParse          p s = shouldParseAsx p s

l </> r = l <> "\n" <> r

spec :: Spec
spec = do
    describe "literals" $ do
        describe "numbers" $ do
                let biggerThanInt64   = show (maxBound :: Int64) <> "0"
                    biggerThanInt64f  = biggerThanInt64  <> "." <> biggerThanInt64
                it "zero"                                       $ shouldParseItself' expr "0"                        [(0,1)]
                it "zero-prefixed ints"                         $ shouldParseItself' expr "007"                      [(0,3)]
                it "unsigned exp notation"                      $ shouldParseItself' expr "187.19e17"                [(0,9)]
                it "positive exp notation"                      $ shouldParseItself' expr "187.19e+17"               [(0,10)]
                it "negative exp notation"                      $ shouldParseItself' expr "187.19e-17"               [(0,10)]
                it "big decimals"                               $ shouldParseItself' expr biggerThanInt64            [(0,20)]
                it "reals"                                      $ shouldParseItself' expr biggerThanInt64f           [(0,41)]
                it "binary"                                     $ shouldParseItself' expr "0b010010100"              [(0,11)]
                it "octal"                                      $ shouldParseItself' expr "0o01234567"               [(0,10)]
                it "hex"                                        $ shouldParseItself' expr "0x0123456789abcdefABCDEF" [(0,24)]
        describe "text" $ do
            describe "raw" $ do
                it "oneliner"                                   $ do shouldParseItself' expr "\"The quick brown fox jumps over the lazy dog\"" [(0,45)]
                it "tripple double-quoted oneliner"             $ do shouldParseAs'     expr [s|"""The quick brown fox jumps over the lazy dog"""|] [s|"The quick brown fox jumps over the lazy dog"|] [(0,49)]
                it "escaping qote"                              $ do shouldParseAs'     expr [s|"foo\""|] [s|"foo""|] [(0,7)] -- FIXME: Fix escaping in pretty printer
                it "escaping newline"                           $ do shouldParseAs'     expr [s|'\n'|] "\"\n\"" [(0,4)]
            describe "interpolated" $ do
                it "oneliner"                                   $ do shouldParseAs      expr "'The quick brown fox jumps over the lazy dog'" "\"The quick brown fox jumps over the lazy dog\""
            --     it "tripple single-quoted oneliner"             $ do shouldParseAs expr     "'''The quick brown fox jumps over the lazy dog'''"    "'The quick brown fox jumps over the lazy dog'"
            --     it "multiline string with inline start"         $ do shouldParseAs expr     "'The quick \n brown fox jumps over the lazy dog'"     "'The quick \nbrown fox jumps over the lazy dog'"
            --     it "multiline string with non-indented newline" $ do shouldParseAs expr     "'The quick \n\n brown fox jumps over the lazy dog'"   "'The quick \n\nbrown fox jumps over the lazy dog'"
            --     it "multiline string with space-only line"      $ do shouldParseAs expr     "'The quick \n  \n brown fox jumps over the lazy dog'" "'The quick \n \nbrown fox jumps over the lazy dog'"
            --     it "multiline string with newline start"        $ do shouldParseAs expr     "'\nThe quick\nbrown fox jumps over the lazy dog\n'"   "'The quick\nbrown fox jumps over the lazy dog'"
            --     it "simple interpolated strings"                $ do shouldParseItself expr "'The quick brown fox jumps over {2 + 2} lazy dogs'"
        describe "lists" $ do
            it "empty list"                                 $ shouldParseItself_raw' expr "[]"                                       [(0,2)]
            it "singleton list"                             $ shouldParseItself_raw' expr "[a]"                                      [(1,1),(0,3)]
            it "few elems list"                             $ shouldParseItself_raw' expr "[a, b, c]"                                [(1,1),(2,1),(2,1),(0,9)]
            it "list section"                               $ shouldParseAs_raw'     expr "[,]" "[, ]"                               [(1,0),(1,0),(0,3)]
            -- it "list with tuple section"                    $ shouldParseItself_raw' expr "[(, )]"                                   [(1,4),(0,6)]
            it "nested lists"                               $ shouldParseItself_raw' expr "[a, [b, c]]"                              [(1,1),(1,1),(2,1),(2,6),(0,11)]
            it "list sections"                              $ shouldParseItself_raw' expr "[, a, , b, ]"                             [(1,0),(2,1),(2,0),(2,1),(2,0),(0,12)]
            -- it "nested section list"                        $ shouldParseItself_raw' expr "[[, ]]"                                   [(1,4),(0,6)]
        describe "tuples" $ do
            it "3-tuple"         $ shouldParseItself' expr "(a, 30, \"ala\")" [(1,1),(2,2),(2,5),(0,14)]
            it "2-tuple section" $ shouldParseItself' expr "(, 30)"           [(1,0),(2,2),(0,6)]

    describe "identifiers" $ do
            it "one letter variable"                        $ shouldParseItself' expr "a"                                        [(0,1)]
            it "variable with trailing apostrophe"          $ shouldParseItself' expr "foo'"                                     [(0,4)]
            it "variable with trailing apostrophes"         $ shouldParseItself' expr "foo''"                                    [(0,5)]
            it "variable with unicode symbols"              $ shouldParseItself' expr "фываΧξωβ김동욱"                           [(0,11)]
            it "wildcard"                                   $ shouldParseItself' expr "_"                                        [(0,1)]
            it "simple constructors"                        $ shouldParseItself' expr "Vector"                                   [(0,6)]
            it "constructors with arguments"                $ shouldParseItself' expr "Vector x 1 z"                             [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(0,12)]
            it "constructor with unicode symbols"           $ shouldParseItself' expr "Κοηστρυκτορ"                              [(0,11)]

    describe "expressions" $ do
        describe "applications" $ do
            it "single arg application"                     $ shouldParseItself' expr "foo a"                                    [(0,3),(1,1),(0,5)]
            it "multiple arg application"                   $ shouldParseItself' expr "foo a b c"                                [(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(0,9)]
            it "applicated parensed expr"                   $ shouldParseItself' expr "foo (a b)"                                [(0,3),(0,1),(1,1),(1,3),(1,5),(0,9)]

        describe "explicite types" $ do
            it "simple var type"                            $ shouldParseItself' expr "a :: t"                                   [(0,1),(4,1),(0,6)]
            it "typed Vector cons"                          $ shouldParseItself' expr "Vector 1 2 3 :: Vector Int"               [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(0,12),(0,6),(1,3),(4,10),(0,26)]
            it "typed local variables"                      $ shouldParseAs'     expr "foo a::A b::B" "foo (a :: A) (b :: B)"    [(0,3),(0,1),(2,1),(1,4),(0,8),(0,1),(2,1),(1,4),(0,13)]
            it "kind typed expression"                      $ shouldParseItself' expr "pi :: Real :: Type"                       [(0,2),(0,4),(4,4),(4,12),(0,18)]
          -- TODO: op prec

        describe "accessors" $ do
            it "single accessors"                           $ shouldParseItself' expr "foo . bar"                                 [(0,3),(0,9)]
            it "chained accessors"                          $ shouldParseItself' expr "foo . bar . baz"                           [(0,3),(0,9),(0,15)]
            it "delayed accessor (single)"                  $ shouldParseItself' expr ".foo"                                      [(0,4)]
            it "delayed accessor (double)"                  $ shouldParseItself' expr ".foo.bar"                                  [(0,8)]
            it "delayed accessor (tripple)"                 $ shouldParseItself' expr ".foo.bar.baz"                              [(0,12)]
            it "delayed accessor (operator)"                $ shouldParseItself' expr ".+"                                        [(0,2)]
            it "delayed accessor (nested op)"               $ shouldParseItself' expr ".foo.+"                                    [(0,6)]
            it "delayed accessor as argument"               $ shouldParseItself' expr "foo .pos"                                  [(0,3),(1,4),(0,8)]
            it "running method of delayed accessor"         $ shouldParseItself' expr ".foo . bar"                                [(0,4),(0,10)]
            it "parensed delayed accessor"                  $ shouldParseItself' expr "(.foo)"                                    [(1,4),(0,6)]
            it "parensed delayed accessor as argument"      $ shouldParseItself' expr "test (.foo)"                               [(0,4),(1,4),(1,6),(0,11)]
            it "operator unspaced accessors"                $ shouldParseAs'     expr "2.+ 1" "2 . + 1"                           [(0,1),(0,3),(1,1),(0,5)]
            it "complex spaced accessors 1"                 $ shouldParseItself' expr "foo 1 . bar 2 3 . baz 4 5"                 [(0,3),(1,1),(0,5),(0,11),(1,1),(0,13),(1,1),(0,15),(0,21),(1,1),(0,23),(1,1),(0,25)]
            it "complex spaced accessors 2"                 $ shouldParseAs'     expr "foo 1 . bar 2.baz 3.ban"
                                                                                      "foo 1 . bar (2 . baz) (3 . ban)"           [(0,3),(1,1),(0,5),(0,11),(0,1),(1,5),(0,17),(0,1),(1,5),(0,23)]

        describe "infixes" $ do
            it "infix operator on variables"                $ shouldParseItself' expr "a + b"                                     [(0,1),(1,1),(0,3),(1,1),(0,5)]
            it "infix operator on applications"             $ shouldParseItself' expr "a b + c d"                                 [(0,1),(1,1),(0,3),(1,1),(0,5),(0,1),(1,1),(1,3),(0,9)]
            it "two infixl operators"                       $ shouldParseItself' expr "a - b - c"                                 [(0,1),(1,1),(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(0,9)]
            it "two infixl operators with spaced prec 1"    $ shouldParseAs'     expr "a - b-c" "a - (b - c)"                     [(0,1),(1,1),(0,3),(0,1),(0,1),(0,2),(0,1),(1,3),(0,7)]
            it "two infixl operators with spaced prec 2"    $ shouldParseAs'     expr "a-b - c" "a - b - c"                       [(0,1),(0,1),(0,2),(0,1),(0,3),(1,1),(0,5),(1,1),(0,7)]
            it "two infixl operators with mixed prec 1"     $ shouldParseItself' expr "a + b * c"                                 [(0,1),(1,1),(0,3),(0,1),(1,1),(0,3),(1,1),(1,5),(0,9)]
            it "two infixl operators with mixed prec 2"     $ shouldParseItself' expr "a * b + c"                                 [(0,1),(1,1),(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(0,9)]
            it "two infixl operators with mixed prec 3"     $ shouldParseAs'     expr "a * b+c" "a * (b + c)"                     [(0,1),(1,1),(0,3),(0,1),(0,1),(0,2),(0,1),(1,3),(0,7)]
            it "two infixl operators with mixed prec 4"     $ shouldParseAs'     expr "a*b + c" "a * b + c"                       [(0,1),(0,1),(0,2),(0,1),(0,3),(1,1),(0,5),(1,1),(0,7)]
            it "space-based precedence"                     $ shouldParseAs'     expr "a - b-c" "a - (b - c)"                     [(0,1),(1,1),(0,3),(0,1),(0,1),(0,2),(0,1),(1,3),(0,7)]
            it "$-separated sub-expressions"                $ shouldParseItself' expr "foo $ bar baz"                             [(0,3),(1,1),(0,5),(0,3),(1,3),(1,7),(0,13)]

        describe "unary minus" $ do
            it "unary minus as expression"                  $ shouldParseItself' expr "-a"                                        [(0,1),(0,1),(0,2)]
            it "unary minus in application"                 $ shouldParseItself' expr "foo -a"                                    [(0,3),(0,1),(0,1),(1,2),(0,6)]
            it "multiple unary minuses in application"      $ shouldParseItself' expr "foo -a 4 -5"                               [(0,3),(0,1),(0,1),(1,2),(0,6),(1,1),(0,8),(0,1),(0,1),(1,2),(0,11)]
            it "binary minus expression"                    $ shouldParseItself' expr "foo - a"                                   [(0,3),(1,1),(0,5),(1,1),(0,7)]
            it "right minus section"                        $ shouldParseAs'     expr "a-" "(a -)"                                [(0,1),(0,1),(0,2)]

        describe "sections" $ do
            it "operator as expression"                     $ shouldParseItself' expr "+"                                         [(0,1)]
            it "border unspaced section (l)"                $ shouldParseAs'     expr "+a"            "(+ a)"                     [(0,1),(0,1),(0,2)]
            it "border unspaced section (r)"                $ shouldParseAs'     expr "a+"            "(a +)"                     [(0,1),(0,1),(0,2)]
            it "border unspaced section (l/r)"              $ shouldParseAs'     expr "+a+"           "((+ a) +)"                 [(0,1),(0,1),(0,2),(0,1),(0,3)]
            it "border spaced section (l)"                  $ shouldParseAs'     expr "+ a"           "(+ a)"                     [(0,1),(1,1),(0,3)]
            it "border spaced section (r)"                  $ shouldParseAs'     expr "a +"           "(a +)"                     [(0,1),(1,1),(0,3)]
            it "border spaced section (l/r)"                $ shouldParseAs'     expr "+ a +"         "((+ a) +)"                 [(0,1),(1,1),(0,3),(1,1),(0,5)]
            it "border section application (l)"             $ shouldParseAs'     expr "+ foo a"       "(+ foo a)"                 [(0,1),(0,3),(1,1),(1,5),(0,7)]
            it "border section application (r)"             $ shouldParseAs'     expr "foo a +"       "(foo a +)"                 [(0,3),(1,1),(0,5),(1,1),(0,7)]
            it "border section application (l/r)"           $ shouldParseAs'     expr "+ foo a +"     "((+ foo a) +)"             [(0,1),(0,3),(1,1),(1,5),(0,7),(1,1),(0,9)]
            it "section l application (l)"                  $ shouldParseAs'     expr "foo +a*2"      "foo (+ a * 2)"             [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(1,4),(0,8)]
            it "section l application (r)"                  $ shouldParseAs'     expr "foo a*2+"      "foo (a * 2 +)"             [(0,3),(0,1),(0,1),(0,2),(0,1),(0,3),(0,1),(1,4),(0,8)]
            it "section l application (l/r)"                $ shouldParseAs'     expr "foo +a*2+"     "foo ((+ a * 2) +)"         [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(0,1),(1,5),(0,9)]
            it "section r application (l)"                  $ shouldParseAs'     expr "+a*2 foo"      "(+ a * 2) foo"             [(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(1,3),(0,8)]
            it "section r application (r)"                  $ shouldParseAs'     expr "a*2+ foo"      "(a * 2 +) foo"             [(0,1),(0,1),(0,2),(0,1),(0,3),(0,1),(0,4),(1,3),(0,8)]
            it "section r application (l/r)"                $ shouldParseAs'     expr "+a*2+ foo"     "((+ a * 2) +) foo"         [(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(0,1),(0,5),(1,3),(0,9)]
            it "section l/r application (l)"                $ shouldParseAs'     expr "foo +a*2 bar"  "foo (+ a * 2) bar"         [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(1,4),(0,8),(1,3),(0,12)]
            it "section l/r application (r)"                $ shouldParseAs'     expr "foo a*2+ bar"  "foo (a * 2 +) bar"         [(0,3),(0,1),(0,1),(0,2),(0,1),(0,3),(0,1),(1,4),(0,8),(1,3),(0,12)]
            it "section l/r application (l/r)"              $ shouldParseAs'     expr "foo +a*2+ bar" "foo ((+ a * 2) +) bar"     [(0,3),(0,1),(0,1),(0,1),(0,2),(0,1),(0,3),(0,4),(0,1),(1,5),(0,9),(1,3),(0,13)]

        describe "groups" $ do
            it "group as expression"                        $ shouldParseItself' expr "(foo)"                                     [(1,3),(0,5)]
            it "group as unspaced left section"             $ shouldParseAs'     expr "(+a)"               "((+ a))"              [(0,1),(0,1),(1,2),(0,4)]
            it "group as unspaced right section"            $ shouldParseAs'     expr "(a+)"               "((a +))"              [(0,1),(0,1),(1,2),(0,4)]
            it "group as unspaced l/r section"              $ shouldParseAs'     expr "(+a+)"              "(((+ a) +))"          [(0,1),(0,1),(0,2),(0,1),(1,3),(0,5)]
            it "group as spaced left section"               $ shouldParseAs'     expr "(+ a)"              "((+ a))"              [(0,1),(1,1),(1,3),(0,5)]
            it "group as spaced right section"              $ shouldParseAs'     expr "(a +)"              "((a +))"              [(0,1),(1,1),(1,3),(0,5)]
            it "group as spaced l/r section"                $ shouldParseAs'     expr "(+ a +)"            "(((+ a) +))"          [(0,1),(1,1),(0,3),(1,1),(1,5),(0,7)]
            it "group with left section"                    $ shouldParseAs'     expr "(+ foo a)"          "((+ foo a))"          [(0,1),(0,3),(1,1),(1,5),(1,7),(0,9)]
            it "group with right section"                   $ shouldParseAs'     expr "(foo a +)"          "((foo a +))"          [(0,3),(1,1),(0,5),(1,1),(1,7),(0,9)]
            it "group with l/r section"                     $ shouldParseAs'     expr "(+ foo a +)"        "(((+ foo a) +))"      [(0,1),(0,3),(1,1),(1,5),(0,7),(1,1),(1,9),(0,11)]
            it "group with complex expression"              $ shouldParseItself' expr "(foo . bar 2 + 11)"                        [(0,3),(0,9),(1,1),(0,11),(1,1),(0,13),(1,2),(1,16),(0,18)]
            it "group application 1"                        $ shouldParseAs'     expr "(foo a)(b)"         "(foo a) (b)"          [(0,3),(1,1),(1,5),(0,7),(1,1),(0,3),(0,10)]
            it "group application 2"                        $ shouldParseItself' expr "(foo a) b (c)"                             [(0,3),(1,1),(1,5),(0,7),(1,1),(0,9),(1,1),(1,3),(0,13)]
            it "group application 3"                        $ shouldParseItself' expr "test (foo a) b (c)"                        [(0,4),(0,3),(1,1),(1,5),(1,7),(0,12),(1,1),(0,14),(1,1),(1,3),(0,18)]
            it "wildcard group application"                 $ shouldParseItself' expr "(foo _) a"                                 [(0,3),(1,1),(1,5),(0,7),(1,1),(0,9)]

        describe "patterns" $ do
            it "variable pattern"                           $ shouldParseItself' expr "a = val"                                   [(0,1),(3,3),(0,7)]
            it "wildcard pattern"                           $ shouldParseItself' expr "_ = val"                                   [(0,1),(3,3),(0,7)]
            it "deconstructor pattern (no args)"            $ shouldParseItself' expr "Vector = val"                              [(0,6),(3,3),(0,12)]
            it "deconstructor pattern (with args)"          $ shouldParseItself' expr "Vector x y z = val"                        [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(0,12),(3,3),(0,18)]
            it "grouped pattern"                            $ shouldParseItself' expr "(Vector x y z) = val"                      [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(1,12),(0,14),(3,3),(0,20)]
            it "module-qualified pattern"                   $ shouldParseAs'     expr "A.B.C x y z = val" "A . B . C x y z = val" [(0,1),(0,3),(0,5),(1,1),(0,7),(1,1),(0,9),(1,1),(0,11),(3,3),(0,17)]

        -- FIXME: Hack, for details see Parsing.hs
        describe "modifiers" $ do
            it "variable's field update"                    $ shouldParseAs' expr "a = a.x = v"  "a = a . x= v"                    [(0,1),(0,1),(0,3),(3,1),(3,7),(0,11)]
            it "variable's field drop update"               $ shouldParseAs' expr "a.x = v"      "a . x= v"                        [(0,1),(0,3),(3,1),(0,7)]
            it "variable's field modification"              $ shouldParseAs' expr "a = a.x += v" "a = a . x+= v"                   [(0,1),(0,1),(0,3),(4,1),(3,8),(0,12)]
            it "variable's field drop modification"         $ shouldParseAs' expr "a.x += v"     "a . x+= v"                       [(0,1),(0,3),(4,1),(0,8)]

        -- FIXME: Correct implementation (vvv). Uncomment when above hack will be invalid.
        -- describe "modifiers" $ do
        --     it "variable's field update"                    $ shouldParseItself' expr "a = a.x = v"                               [(0,1),(0,1),(5,1),(3,7),(0,11)]
        --     it "variable's field drop update"               $ shouldParseItself' expr "a.x = v"                                   [(0,1),(5,1),(0,7)]
        --     it "variable's field modification"              $ shouldParseItself' expr "a = a.x += v"                              [(0,1),(0,1),(6,1),(3,8),(0,12)]
        --     it "variable's field drop modification"         $ shouldParseItself' expr "a.x += v"                                  [(0,1),(6,1),(0,8)]


        describe "lambdas" $ do
            it "identity (not spaced)"                      $ shouldParseAs'     expr "x:x"  "x: x"                               [(0,1),(1,1),(0,3)]
            it "identity (l   spaced)"                      $ shouldParseAs'     expr "x :x" "x: x"                               [(0,1),(2,1),(0,4)]
            it "identity (r   spaced)"                      $ shouldParseItself' expr "x: x"                                      [(0,1),(2,1),(0,4)]
            it "identity (l/r spaced)"                      $ shouldParseAs'     expr "x : x" "x: x"                              [(0,1),(3,1),(0,5)]
            it "double lambda"                              $ shouldParseItself' expr "a: b: a + b"                               [(0,1),(0,1),(0,1),(1,1),(0,3),(1,1),(2,5),(2,8),(0,11)]
            it "complex pattern lambda"                     $ shouldParseItself' expr "(Vector x y z): x + y + z"                 [(0,6),(1,1),(0,8),(1,1),(0,10),(1,1),(1,12),(0,14),(0,1),(1,1),(0,3),(1,1),(0,5),(1,1),(0,7),(1,1),(2,9),(0,25)]
            it "wildcard arg lambda"                        $ shouldParseItself' expr "_: 1"                                      [(0,1),(2,1),(0,4)]
            it "double wildcard lambda"                     $ shouldParseItself' expr "_: _"                                      [(0,1),(2,1),(0,4)]
            it "lambda as parensed argument"                $ shouldParseItself' expr "foo (x: x + 1)"                            [(0,3),(0,1),(0,1),(1,1),(0,3),(1,1),(2,5),(1,8),(1,10),(0,14)]
            it "lambda as $-argument"                       $ shouldParseItself' expr "foo $ x: x + 1"                            [(0,3),(1,1),(0,5),(0,1),(0,1),(1,1),(0,3),(1,1),(2,5),(1,8),(0,14)]

        describe "function definition expressions" $ do
            it "no argument function definition"            $ shouldParseItself' expr "def foo: bar"                              [(4,3),(2,3),(0,12)]
            it "simple function definition"                 $ shouldParseItself' expr "def foo a b: a + b"                        [(4,3),(1,1),(1,1),(0,1),(1,1),(0,3),(1,1),(2,5),(0,18)]

        describe "top level definitions" $ do
            it "value definition"                           $ shouldParseAs''     unit' "def pi: 3.14"       "<function 'pi'>"       [(4,2),(0,12),(0,12),(0,12)]
            it "expression definition"                      $ shouldParseAs''     unit' "def foo: a + b"     "<function 'foo'>"      [(4,3),(0,14),(0,14),(0,14)]
            it "function definition"                        $ shouldParseAs''     unit' "def foo a b: a + b" "<function 'foo'>"      [(4,3),(0,18),(0,18),(0,18)]
            it "operator definition"                        $ shouldParseAs''     unit' "def + a b: a.+ b"   "<function '+'>"        [(4,1),(0,16),(0,16),(0,16)]
            it "function signature definition"              $ shouldParseItself'' unit' "def foo :: a -> Vector a"                   [(4,3),(0,1),(1,2),(0,4),(0,6),(1,1),(1,8),(4,13),(0,24),(0,24),(0,24)]

        describe "case expression" $ do
            it "simple case expression"                     $ shouldParseItself' expr "case v of\n    A a: b" [(5,1),(0,1),(1,1),(0,3),(2,1),(8,6),(0,20)]
            it "multiline case expression"                  $ shouldParseItself' expr ("case v of"
                                                                                   </> "    A: a"
                                                                                   </> "    B: b"
                                                                                      ) [(5,1),(0,1),(2,1),(8,4),(0,1),(2,1),(5,4),(0,27)]

        describe "imports" $ do
            it "import everything"                          $ shouldParseItself' unit' "import Std.Math"                         [(7,8),(0,15),(0,15),(0,15)]
            it "import selected"                            $ shouldParseItself' unit' "import Std.Math: Vector Scalar"          [(7,8),(0,30),(0,30),(0,30)]
            it "import relative"                            $ shouldParseItself' unit' "import .Local"                           [(7,6),(0,13),(0,13),(0,13)]
            it "import from World"                          $ shouldParseAs'     unit' "import World: Std" "import #World#: Std" [(7,5),(0,17),(0,17),(0,17)]

        describe "units" $ do
            it "empty unit"                                 $ shouldParseItself' unit' ""                                        []
            it "unit with newlines on the end"              $ shouldParseAs'     unit' "def foo: bar\n\n\n" "\n<function 'foo'>" [(4,3),(0,12),(0,12),(0,12)]
            it "unit with newlines on the beginning"        $ shouldParseAs'     unit' "\n\n\ndef foo: bar" "\n<function 'foo'>" [(3,0),(4,3),(0,12),(0,12),(0,15)]

        describe "classes" $ do
            it "phantom class"                              $ shouldParseItself'' unit' "class Phantom"                                               [(0,13),(0,13),(0,13)]
            it "no cons and fields class"                   $ shouldParseAs'      unit' "class C:\n    def foo: a" "\nclass C:\n    <function 'foo'>" [(4,3),(13,10),(0,23),(0,23),(0,23)]
            it "constructor-only class"                     $ shouldParseItself'' unit' ("class Bool:"
                                                                                     </> "    True"
                                                                                     </> "    False"
                                                                                        ) [(16,4),(5,5),(0,30),(0,30),(0,30)]

            it "implicite constructor, parametrized class"  $ shouldParseItself'' unit' ("class Vector a:"
                                                                                     </> "    x y z :: a"
                                                                                        ) [(13,1),(9,1),(6,10),(0,30),(0,30),(0,30)]

            -- FIXME [WD]: broken pretty printer here
            it "complex class"                              $ shouldParse          unit' ("class Vector a:"
                                                                                     </> "    V a a a"
                                                                                     </> "    X: fld :: a"
                                                                                     </> "    def foo: 0"
                                                                                        ) [(13,1),(0,1),(2,1),(0,1),(1,1),(0,1),(1,1),(6,7),(7,1),(3,8),(5,11),(4,3),(5,10),(0,58),(0,58),(0,58)]
        describe "block layout" $ do
          -- TODO: prawdziwe bloki
            let lam1  = "lam: foo"
                    </> "     bar"
                lam1' = "lam:"
                    </> "    foo"
                    </> "    bar"
            it "inline block"                               $ shouldParseAs' expr lam1 lam1' [(0,3),(0,3),(6,3),(2,12),(0,17)]
            it "newline block"                              $ shouldParseItself' expr lam1'  [(0,3),(0,3),(5,3),(6,11),(0,20)]
            it "nested blocks"                              $ shouldParseItself' expr ( "a:"
                                                                                    </> "    foo"
                                                                                    </> "    bar b:"
                                                                                    </> "        baz"
                                                                                    </> "        bax"
                                                                                    </> "    bam"
                                                                                      ) [(0,1),(0,3),(0,3),(0,1),(0,3),(9,3),(10,15),(1,26),(5,30),(0,38),(5,3),(6,46),(0,53)]

           -- TODO: structure updates
           -- describe "structure updates" $ do
           --     it "field setting"                 $ shouldParseItself expr "foo.x = 5"
           --     it "field modification (unspaced)" $ shouldParseItself expr "foo.x += 5"
           --     it "field modification (spaced)"   $ shouldParseItself expr "foo . x += 5"
           --     it "field modification (lens)"     $ shouldParseItself expr "vec . at pos += 5"


        describe "markers" $ do
            it "marked expression"                       $ shouldParseItself' expr "«0»foo bar" [(0,3),(0,3),(1,3),(0,7),(0,10)]
            it "marked assignment expression"            $ shouldParseItself' expr "«0»a = foo" [(0,3),(0,1),(3,3),(0,7),(0,10)]

        describe "mixfixes" $ do
            it "mixfix (if .. then .. else)"             $ shouldParseItself' expr "if a b then c d else e f" [(0,2),(0,1),(1,1),(1,3),(0,6),(0,1),(1,1),(6,3),(0,15),(0,1),(1,1),(6,3),(0,24)]

        describe "disabled" $ do
            it "disabled var expression"                 $ shouldParseItself' expr "#foo"                  [(1,3),(0,4)]
            it "disabled app expression"                 $ shouldParseItself' expr "#foo bar"              [(0,3),(1,3),(1,7),(0,8)]
            it "disabled multiline lambda"               $ shouldParseItself' expr "#a:\n    foo\n    bar" [(0,1),(0,3),(5,3),(6,11),(1,18),(0,19)]

        describe "documentation" $ do
            it "single line doc comment"                 $ shouldParseItself' expr "def foo a: a" [(4,3),(1,1),(2,1),(0,12)]

        describe "metadata" $ do
            it "metadata line"                           $ shouldParseItself'' unit' "### META {\"0\": {\"studio\": ...}}" [(0,31),(0,31),(0,31)]


-- foo . pos . x += 1
--           . y += 2
