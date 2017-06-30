{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedLists      #-}

module Main where

import qualified Prelude as P
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Luna.Prelude as Prelude hiding (String, cons, elem, (|>), (<|), length, span, empty)
import qualified Luna.Prelude as Prelude
import Data.Aeson (encode)
import           Luna.IR hiding (Elem)
import qualified OCI.IR.Repr.Vis as Vis
import           OCI.IR.Repr.Vis (MonadVis)
import           OCI.Pass        (Pass, SubPass, Preserves, Events, Inputs, Outputs)
import qualified OCI.Pass        as Pass
import           OCI.IR.Layout.Typed (type (>>), type (:>))
import qualified OCI.IR.Layout.Typed as Layout
import qualified Data.TreeMap as TM
import Web.Browser (openBrowser )
import OCI.IR.Term
import qualified Data.Set as Set
import Data.Set (Set)
import Control.Monad.State.Dependent
import OCI.Pass.Manager as PM
import Data.Event as Event
import qualified Data.ManagedVectorMap as Store
import Data.ManagedVectorMap (STRefM)
import Luna.IR.Layer.UID (ID)
import qualified Luna.IR.Expr as Term
import System.Log
import System.Log.Logger.Format (nestedColorFormatter)
import GHC.Stack
import Control.Concurrent
import System.Exit
import qualified Data.Graph.Class as Graph
import Control.Monad.Raise
import qualified Luna.Syntax.Text.Parser.Parsing as Parsing
import qualified Luna.Syntax.Text.Layer.Loc as Loc
import Data.TypeDesc
import qualified Luna.Syntax.Text.Parser.Parser   as Parser
import           Luna.Syntax.Text.Parser.Parser   (Parsing, ParsedExpr, ReparsingStatus)
import qualified Luna.Syntax.Text.Parser.CodeSpan as CodeSpan
import           Luna.Syntax.Text.Parser.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Marker (MarkedExprMap)
import Data.Text.Position
import Luna.Syntax.Text.Source
import Data.String.Utils hiding (split)
import qualified Data.Map as Map
import           Data.Map (Map)
import Luna.IR.Term.World (World, initWorld)
import Luna.IR.Term.Unit (UnitSet)
import qualified Data.TreeSet as TreeSet
import Luna.Syntax.Text.Parser.Errors (Invalids)

import qualified Data.FingerTree as FT
import           Data.FingerTree (Measured, FingerTree, takeUntil, dropUntil, measure)
import Data.Monoid (Sum(Sum))
import qualified Data.Text as Text
import qualified Luna.Syntax.Text.Lexer as Lexer
import qualified Data.VectorText as VectorText
import           Data.VectorText (VectorText)

import qualified Luna.Syntax.Text.Pretty.Pretty as CodeGen
import qualified Data.Text.Span as Span

import qualified Data.Layout as D
import Luna.Syntax.Text.SpanTree

data ShellTest
type instance Abstract ShellTest = ShellTest
type instance Inputs  Net   ShellTest = '[AnyExpr]
type instance Outputs Net   ShellTest = '[AnyExpr]
type instance Inputs  Layer ShellTest = '[AnyExpr // Model, AnyExpr // UID, Link' AnyExpr // UID, Link' AnyExpr // Model, AnyExpr // Succs, AnyExpr // CodeSpan]
type instance Outputs Layer ShellTest = '[]
type instance Inputs  Attr  ShellTest = '[ReparsingStatus, WorldExpr, ParsedExpr, Source, MarkedExprMap]
type instance Outputs Attr  ShellTest = '[]
type instance Inputs  Event ShellTest = '[] -- will never be used
type instance Outputs Event ShellTest = '[New // AnyExpr]
type instance Preserves     ShellTest = '[]


test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m, Logging m, Throws '[RefLookupError, IRError, PassEvalError] m) => m ()
test_pass1 = evalDefStateT @Cache $ evalIRBuilder' $ evalPassManager' $ do
    return ()
    runRegs' False

    Loc.init
    attachLayer 5 (getTypeDesc @Range) (getTypeDesc @AnyExpr)

    CodeSpan.init
    attachLayer 5 (getTypeDesc @CodeSpan) (getTypeDesc @AnyExpr)

    -- ParserLoader.init

    setAttr (getTypeDesc @MarkedExprMap) $ (mempty :: MarkedExprMap)
    setAttr (getTypeDesc @ParsedExpr)    $ (error "Data not provided: ParsedExpr")
    setAttr (getTypeDesc @Invalids)      $ (mempty :: Invalids)

    setAttr (getTypeDesc @ReparsingStatus) $ (mempty :: ReparsingStatus)

    -- setAttr (getTypeDesc @Source) $ ("main:\n    «0»pi = 5\n    «1»a = 60" :: Source)
    setAttr (getTypeDesc @Source) $ ("«0»foo bar" :: Source)
    -- setAttr (getTypeDesc @Source) $ ("main :   «1777»a" :: Source)

    -- World initialization
    setAttr (getTypeDesc @WorldExpr) (undefined :: WorldExpr) -- FIXME[WD]: it's ugly, we have to find a way to initialize attrs by pass manager
    Pass.eval' initWorld

    Pass.eval' (Parsing.parserPassX Parsing.expr)

    -- Unit initialization
    -- setAttr (getTypeDesc @UnitSet) ( [ ("MyLib", [ ["Main"] ])
    --                                  , ("Std"  , [ ["Math", "Vector"]
    --                                              , ["A", "B", "C"]
    --                                              ]
    --                                    )
    --                                  ] :: UnitSet)
        -- setAttr (getTypeDesc @UnitSet) ( [ ("MyLib", [ ["Main"] ])
        --                                  , ("Std"  , [ ["A"]    ])
        --                                  ] :: UnitSet)
        -- Pass.eval' @UL.UnitInitializer UL.runUnitInitializer
        --
        -- -- Preparing units to load
        -- setAttr (getTypeDesc @UL.UnitsToLoad) (mempty :: UL.UnitsToLoad)
        -- setAttr (getTypeDesc @UL.UnitsToLoadRequest) (wrap [["MyLib", "Main"]] :: UL.UnitsToLoadRequest)
        -- Pass.eval' @UL.UnitRequester UL.unitRequester
        --
        -- -- Unit loading
        -- setAttr (getTypeDesc @UL.SourcesManager) (UL.fsSourceManager $ Map.insert ["MyLib", "Main"] (UL.Source "/home/wdanilo/dev/tmp/Main.luna" "/home/wdanilo/dev/tmp/Main.lunai")
        --                                                              $ Map.insert ["Std"  , "A"   ] (UL.Source "/home/wdanilo/dev/tmp/A.luna"    "/home/wdanilo/dev/tmp/A.lunai")
        --                                                              $ mempty
        --                                          )
        -- Pass.eval' @UL.UnitLoader      UL.unitLoader
        -- Pass.eval' @UL.ImportsResolver UL.importsResolver


    Pass.eval' @ShellTest $ do
        Vis.snapshotNoType "after parsing"


        src    <- unwrap <$> getAttr @Source
        result <- unwrap <$> getAttr @ParsedExpr
        print result
        -- putStrLn "\nReparsingStatus:"
        -- pprint =<< getAttr @ReparsingStatus


            -- putStrLn "\n--- === SrcTree === ---\n"
            -- pprint srcTree
            -- pprint $ SpanTree.bottomSpans srcTree
            -- putStrLn "\n--- === SrcTree === ---\n"
            -- pprint srcTree
            -- pprint $ SpanTree.bottomSpans srcTree
            --
        putStrLn "\n--- === CodeSpans === ---\n"
        es <- exprs
        ls <- getLayer @CodeSpan <$$> es
        pprint $ zip es ls

        -- putStrLn "\n--- === Abs CodeSpans === ---\n"
        -- es   <- exprs
        -- acss <- CodeSpan.absSpan <$$> es
        -- pprint $ zip es acss

        putStrLn "\n--- === CodeSpan split === ---\n"
        begin <- CodeSpan.viewToRealOffsetWithMarkers_ 1 result
        end   <- CodeSpan.viewToRealOffset_            4 result
        print (begin, end)
        putStrLn $ "\"" <> convert (VectorText.replace (convert begin) (convert end) "foo" src) <> "\""

        putStrLn "\n--- === Codegen === ---\n"
        putStrLn . convert =<< CodeGen.subpass CodeGen.SimpleStyle (unsafeGeneralize result)

        putStrLn "\n--- === Marker map === ---\n"
        gidMap <- unwrap <$> getAttr @MarkedExprMap
        pprint gidMap




uncheckedDeleteStar :: (MonadRef m, Reader Layer (AnyExpr // Type) m, Editors Net '[Link' AnyExpr, AnyExpr] m) => Expr l -> m ()
uncheckedDeleteStar e = do
    freeElem =<< getLayer @Type e
    freeElem e
{-# INLINE uncheckedDeleteStar #-}

uncheckedDeleteStarType :: (MonadRef m, Reader Layer (AnyExpr // Type) m, Editors Net '[Link' AnyExpr, AnyExpr] m, Editors Layer '[Link' AnyExpr // Model] m)
                        => Expr l -> m ()
uncheckedDeleteStarType e = do
    typeLink     <- getLayer @Type e
    (oldStar, _) <- getLayer @Model typeLink
    uncheckedDeleteStar oldStar
    freeElem typeLink
{-# INLINE uncheckedDeleteStarType #-}



--
-- main :: HasCallStack => IO ()
-- main = do
--     -- D.main
--     runTaggedLogging $ runEchoLogger $ runFormatLogger nestedColorFormatter $ do
--         (p, vis) <- Vis.newRunDiffT $ tryAll test_pass1
--         case p of
--             Left  e -> critical $ convert $ displayException e
--             Right _ -> do
--                 let cfg = replace "#" "%23" $ ByteString.unpack $ encode $ vis
--                 -- putStrLn cfg
--                 -- liftIO $ openBrowser ("http://localhost:8000?cfg=" <> cfg)
--                 return ()





main :: IO ()
main = do
    let input :: VectorText
        -- input = "«0»Vector x y z = v\n«1»Scalar a = t"
        input = "«0»Vector x y z = v"
        -- input = "«0"
    let stream = Lexer.runLexer input :: [Lexer.LexerToken (Lexer.Symbol Name)]
        st     = buildSpanTree input stream
    -- pprint stream
    -- pprint st
    -- -- putStrLn $ convert (convert stream :: Text)
    -- -- Lexer.main
    -- -- C2.mainc
    -- let
    --     st' = insertText 1 "!"  st
    -- --     st' = breakLine 17 st
    -- --     -- st = empty |> Spanned (Span 1 1) " " |> Spanned (Span 3 0) "«0»" |> Spanned (Span 7 7) "def foo"
    -- -- print $ measure st
    -- -- let i = 14
    -- -- pprint $ splitAtViewOffset True i st
    -- -- pprint $ viewToRealBlock st (i, i+1)
    -- -- putStrLn "---"
    -- pprint st'
    -- let left = 1
    --     right = 2
    --     (len, (shift, pre, post)) = viewToRealCursorSplitAfterMarker st left
    --     r' = viewToRealCursorBeforeMarker post (shift + right - left)

    print $ viewToRealBlock st (1,2)
    -- pprint $ viewToRealCursorSplitAfterMarker st 1
    -- pprint post
    -- pprint (shift + right - left)
    -- pprint (r' + len)
    -- putStrLn . convert $ mconcat' st'
    putStrLn "---"
    --
-- viewToRealCursorSplitAfterMarker  ::         Spantree a -> Delta -> (Delta, (Delta, Spantree a, Spantree a))

--
-- --  P | S |
-- -- [ ] [ ] expr -> assignment (foo bar) -> (v = foo bar) - kursor PRZED znacznikiem
-- -- [ ] [ ] expr -> expr       (bar)     -> (foo bar)     - kursor PO    znaczniku
-- --
-- --
-- --
-- --
-- --
-- --


-- replaceBlock :: (Delta, Delta) -> Spantree a -> (Spantree a -> Spantree a)
-- replaceBlock (l, r) st =


    -- backspace :: Delta -> Spantree Text -> Spantree Text
    -- backspace d st = pre' <> post where
    --     ((_, shift), (pre, post)) = viewToRealCursorSplit False st d
    --     betweenSegments           = shift == mempty
    --     pre'                      = case splitLast pre of
    --         Just (last, init) -> init |> initSpannedText last
    --         Nothing           -> mempty


    -- splitSpannedText :: Int -> Spanned Text -> (Spanned Text, Spanned Text)

    -- insertIntoSpannedText :: Int -> Text -> Spanned Text -> Spanned Text

    -- buildSpanTree  ::          Text -> [Lexer.LexerToken (Lexer.Symbol Name)] -> Spantree Text
    -- buildSpanTree' :: Delta -> Text -> [Lexer.LexerToken (Lexer.Symbol Name)] -> Spantree Text
    -- buildSpanTree = buildSpanTree' mempty
    -- buildSpanTree' i src = \case
    --     []     -> addCurrent empty
    --     (t:ts) -> case t ^. Lexer.element of
    --         Lexer.Marker m -> addCurrent $ markerSpanned span ms <| buildSpanTree' off mss ts
    --             where (ms, mss) = Text.splitAt (convert span) ss
    --         _ -> buildSpanTree' (i <> diff) src ts
    --         where span = t ^. Lexer.span
    --               off  = t ^. Lexer.offset
    --               diff = span + off
    --     where addCurrent = if i /= mempty then (textSpanned i s <|) else id
    --           (s,ss)     = Text.splitAt (convert i) src
