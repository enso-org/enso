{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module InterpreterSpec (spec) where

import           Prelude.Luna                                    hiding (Num)

import           Control.Monad                                   (forM_)
import           Data.Construction
import           Old.Data.Prop
import           Data.Record                                     hiding (cons)
import           Data.Version.Semantic                           (showVersion, version)
import           Type.Inference

import           Data.Graph
import           Data.Graph.Builder
import qualified Luna.Env.Env                      as Env
import qualified Old.Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Old.Luna.Pretty.GraphViz
import           Old.Luna.Runtime.Dynamics                         (Static)
import           Old.Luna.Syntax.Term.Class                            hiding (source)
import           Old.Luna.Syntax.Model.Layer
import           Old.Luna.Syntax.Model.Network.Builder               (Sign (..), tcErrors)
import           Old.Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Old.Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Old.Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Old.Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Old.Luna.Syntax.Model.Network.Term

import           Luna.Pass.Evaluation.Interpreter.Layer         (InterpreterData (..), InterpreterLayer)
import qualified Luna.Pass.Evaluation.Interpreter.Layer         as Layer
import qualified Luna.Pass.Evaluation.Interpreter.Value         as Value
import qualified Luna.Pass.Evaluation.Interpreter.Interpreter   as Interpreter

import qualified Data.Graph.Backend.NEC                          as NEC
import qualified Data.Graph.Builder.Class                        as Graph
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit

import           Control.Monad.Catch         (Exception, catchAll)

import           Luna.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Pass.Inference.Unification     (StrictUnificationPass (..))
import           Luna.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Pass.Inference.Importing       (SymbolImportingPass (..))
import           Luna.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Pass.Utils.Literals            as LiteralsUtils
import qualified Old.Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Old.Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Old.Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import qualified Control.Monad.Writer                            as Writer
import qualified StdLibMock                                      as StdLib
import qualified Luna.IR.Library.Symbol                             as Symbol
import           Old.Control.Monad.Event                             (Dispatcher)

import           Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, expectationFailure)
import           System.IO.Silently (silence)



graph1 :: forall term node edge nr er ls m n e c. ( term ~ Draft Static
                                                  , node ~ (ls :<: term)
                                                  , edge ~ Link (ls :<: term)
                                                  , nr   ~ Ref Node node
                                                  , er   ~ Ref Edge edge
                                                  , BiCastable     n (ls :<: term)
                                                  , BiCastable     e edge
                                                  , MonadIO       m
                                                  , NodeInferable m (ls :<: term)
                                                  , TermNode Lit.Star   m (ls :<: term)
                                                  , TermNode Lit.Number m (ls :<: term)
                                                  , TermNode Lit.String m (ls :<: term)
                                                  , TermNode Var        m (ls :<: term)
                                                  , TermNode Acc        m (ls :<: term)
                                                  , TermNode App        m (ls :<: term)
                                                  , HasProp InterpreterData (ls :<: term)
                                                  , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer
                                                  , Graph.MonadBuilder (Hetero (NEC.Graph n e c)) m
                                                  )
       => m ([nr])
graph1 = do
    j1 <- int 6
    j2 <- int 7
    plus <- str "+"
    appPlus0 <- app plus [arg j1, arg j2]
    return [appPlus0]

graph1' = do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    accPlus1a  <- acc "+" i1
    appPlus1a  <- app accPlus1a [arg i2]

    accPlus1b  <- acc "+" i3
    appPlus1b  <- app accPlus1b [arg appPlus1a]
    return [appPlus1b]

graph1'' = do
    s1 <- str "abc"
    s2 <- str "def"
    s3 <- str "ghi"
    accConc1a  <- acc "+" s2
    appConc1a  <- app accConc1a [arg s1]

    accConc1b  <- acc "+" appConc1a
    appConc1b  <- app accConc1b [arg s3]

    accLen     <- acc "length" appConc1b
    appLen     <- app accLen []

    return [appLen]

graph1''' = do
    [appPlus1b] <- graph1'
    [appLen] <- graph1''
    accPlus2  <- acc "+" appPlus1b
    appPlus2  <- app accPlus2 [arg appLen]
    return [appPlus2]

prebuild :: IO (Ref Node (NetLayers :<: Draft Static), NetGraph)
-- prebuild :: IO (Ref Node (NetLayers :<: Draft Static), Hetero (NEC.Graph n e c))
prebuild = runBuild def star

-- runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
runBuild (g :: Hetero (NEC.Graph n e c)) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Draft Static)))
                             $ runNetworkBuilderT g m

evalBuild = fmap snd ∘∘ runBuild


test_old :: IO ()
test_old = do
    putStrLn "Interpreter test"
    (_,  g00 :: NetGraph) <- prebuild
    flip catchAll (\e -> return ()) $ flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v
        flip catchAll (\e -> putStrLn $ show e) $ TypeCheck.runT $ do
            (refsToEval, g01) <- runBuild  g00 graph1
            g02               <- evalBuild g01 $ Interpreter.run refsToEval
            renderAndOpen [ ("g2", "g2", g02)
                          ]
    putStrLn "done"

-- 1.+ 2
graph3 = do
    i1 <- int 1
    i2 <- int 2
    apl <- acc "+" i1
    apppl <- app apl [arg i2]

    return [apppl]

graph3' = do
    s1 <- str "hi"
    id <- var "id"
    appid <- app id [arg s1]
    return [appid]

graph4 = do
    i1 <- int 2
    i2 <- int 3

    act <- acc "times" i1
    apt <- app act [arg i2]

    ach <- acc "head" apt
    aph <- app ach []

    return [aph]

graph5 = do
    iLen <- int 10
    iVal <- int 3
    bl   <- blank
    fun  <- acc "succ" bl

    act <- acc "times" iLen
    apt <- app act [arg iVal]

    acm <- acc "map" apt
    apm <- app acm [arg fun]

    ach <- acc "head" apm
    aph <- app ach []

    return [aph]

graph6 = do
    iLen  <- int 5
    iVal  <- int 3
    iInit <- int 1
    fun   <- var "+"

    act <- acc "times" iLen
    apt <- app act [arg iVal]

    acf <- acc "fold" apt
    apf <- app acf [arg iInit, arg fun]

    return [apf]

collectGraph :: forall (m :: * -> *) n t.
                (TypeCheckState.MonadTypeCheck n m, MonadBuilder t m,
                 Writer.MonadWriter [([Char], t)] m, MonadIO m) =>
                [Char] -> m ()
collectGraph tag = do
    putStrLn $ "after pass: " ++ tag
    tcState <- TypeCheckState.get
    putStrLn $ "TCState is: " ++ show tcState
    g <- Graph.get
    Writer.tell [(tag, g)]

seq3 :: forall a a1 b. a -> a1 -> b -> Sequence a (Sequence a1 b)
seq3 a b c = Sequence a $ Sequence b c
seq4 :: forall a a1 a2 b.
        a -> a1 -> a2 -> b -> Sequence a (Sequence a1 (Sequence a2 b))
seq4 a b c d = Sequence a $ seq3 b c d

newtype LunaEvaluationException = LunaEvaluationException [String]
    deriving (Eq, Show, Typeable)
instance Exception LunaEvaluationException

data LunaTypecheckingException = LunaTypecheckingException
    deriving (Eq, Show, Typeable)
instance Exception LunaTypecheckingException

lunaEvaluationException :: LunaEvaluationException -> Bool
lunaEvaluationException = const True

lunaTypecheckingException :: LunaTypecheckingException -> Bool
lunaTypecheckingException = const True

equals :: (Value.FromData a, Eq a, Show a) => Value.Value -> a -> IO ()
equals v expected = do
    v' <- Value.toIO v
    Value.unsafeFromData v' `shouldBe` expected

oneAndOnlyTypecheckerFlow :: Sequence
                               ScanPass
                               (Sequence
                                  LiteralsPass
                                  (Sequence
                                     StructuralInferencePass
                                     (Loop
                                        (Sequence
                                           (Loop
                                              (Sequence
                                                 SymbolImportingPass
                                                 (Sequence
                                                    (Loop StrictUnificationPass)
                                                    (Sequence
                                                       FunctionCallingPass
                                                       (Loop StrictUnificationPass)))))
                                           StrictUnificationPass))))
oneAndOnlyTypecheckerFlow = seq4
     ScanPass
     LiteralsPass
     StructuralInferencePass
     (Loop $ Sequence
         (Loop $ seq4
             SymbolImportingPass
             (Loop $ StrictUnificationPass Positive False)
             FunctionCallingPass
             (Loop $ StrictUnificationPass Positive False))
         (StrictUnificationPass Negative True))

extract :: (MonadThrow m) => Layer.ValueErr Value.Value -> m Value.Value
extract v = case v of
    Left v' -> throwM $ LunaEvaluationException v'
    Right v' -> return v'

typecheckGraph :: _
typecheckGraph graph = do
    (_, g :: NetGraph) <- prebuild

    flip Env.evalT def $
        TypeCheck.runT $ do
            ([root], gb) <- runBuild g graph

            (_, tcGraph) <- runBuild gb $ do
                Symbol.loadFunctions StdLib.symbols
                TypeCheckState.modify_ $ TypeCheckState.freshRoots .~ [root]
                TypeCheck.runTCPass oneAndOnlyTypecheckerFlow

            (foobar, _) <- runBuild tcGraph $
                follow (prop TCData . tcErrors) root

            when (not $ null foobar) $ throwM LunaTypecheckingException

            return (tcGraph, root)

evaluateGraph :: _
evaluateGraph graph = do
    (tcGraph, root) <- typecheckGraph graph

    flip Env.evalT def $ do
        gint <- intRun tcGraph [root]

        (output, _) <- runBuild gint $ do
            follow (prop InterpreterData . Layer.value) root

        extract output

evaluatesTo :: forall a. (Value.FromData a, Show a, Eq a) => _ -> a -> IO ()
evaluatesTo graph expected = do
    res <- evaluateGraph graph
    equals @a res expected

throws graph expected = evaluateGraph graph `shouldThrow` expected

graph2Plus2 :: _
graph2Plus2 = do
    two <- int 2
    two' <- int 2
    twoplus <- acc "+" two
    apl <- app twoplus [arg two']
    return [apl]

graph2Plus2Fancy :: _
graph2Plus2Fancy = do
    two <- int 2
    two' <- int 2
    a <- var "app"
    bl <- blank
    accpl <- acc "+" bl
    bl2 <- blank
    apl <- app accpl [arg bl2, arg two]
    apl' <- app a [arg apl, arg two']
    return [apl']

intRun :: ( MonadFix m
          , MonadIO m
          , Dispatcher ELEMENT (Ptr Node ('Known (NetLayers :<: Draft Static))) m
          , Dispatcher CONNECTION (Ptr Edge ('Known (Arc (NetLayers :< Draft Static NetLayers) (NetLayers :< Draft Static NetLayers)))) m
          )
       => NetGraph -> [Ref Node (NetLayers :<: Draft Static)] -> m NetGraph
intRun gtc refsToEval = do
    gint <- evalBuild gtc $ Interpreter.run refsToEval
    return gint

spec :: Spec
spec = do
    describe "interprets" $ do
        it "2.+ 2" $
            graph2Plus2 `evaluatesTo` (4 :: Int)
        it "app ((_.+) _ 2) 2" $
            graph2Plus2Fancy `evaluatesTo` (4 :: Int)
        it "(5.times 3).fold 1 (+)" $
            graph6 `evaluatesTo` (16 :: Int)
        it "((10.times 3).map _.succ).head" $
            graph5 `evaluatesTo` (Just 4 :: Maybe Int)
        it "(2.times 3).head" $
            graph4 `evaluatesTo` (Just 3 :: Maybe Int)
        it "1.+ 2" $
            graph3 `evaluatesTo` (3 :: Int)
        it "id \"hi\"" $
            graph3' `evaluatesTo` ("hi" :: String)
        it "(2.+ 3).+ 4" $
            graph1' `evaluatesTo` (9 :: Int)
        it "((\"abc\".+ \"def\").+ \"ghi\").length" $
            graph1'' `evaluatesTo` (9 :: Int)
    describe "does not typecheck" $ do
        it "\"+\" 6 7" $ do
            graph1 `throws` lunaTypecheckingException

-- old version


-- {-# LANGUAGE DataKinds                 #-}
-- {-# LANGUAGE FunctionalDependencies    #-}
-- {-# LANGUAGE GADTs                     #-}
-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE NoOverloadedStrings       #-}
-- {-# LANGUAGE OverloadedStrings         #-}
-- {-# LANGUAGE PartialTypeSignatures     #-}
-- {-# LANGUAGE RankNTypes                #-}
-- {-# LANGUAGE RecursiveDo               #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE TupleSections             #-}
-- {-# LANGUAGE TypeFamilies              #-}
-- {-# LANGUAGE TypeOperators             #-}
-- {-# LANGUAGE UndecidableInstances      #-}

-- module Main where

-- import           Prologue                     hiding (Cons, Indexable, Ixed, Repr, Simple, children, cons, empty, index, lookup, maxBound,
--                                                minBound, repr, s, simple)

-- import           Control.Monad.Trans.Except
-- import           Control.Monad.Trans.Identity
-- import           Data.Layer_OLD.Coat
-- import qualified Data.Map                     as Map
-- import           Data.Variants                hiding (cons)
-- import           Data.Vector.Mutable          ()
-- import           Debug.Trace
-- import           Luna.Diagnostic.AST          as Diag (open, render, toGraphViz)
-- import           Luna.Pass.Evaluation.Interpreter.Label       (Label)
-- import qualified Luna.Pass.Evaluation.Interpreter.NodeRunner  as NodeRunner
-- import qualified Luna.Pass.Evaluation.Interpreter.Session     as Session
-- import           Old.Luna.Syntax.Term.Class
-- import           Old.Luna.Syntax.Builder
-- import qualified Old.Luna.Syntax.Builder          as Builder
-- import qualified Old.Luna.Syntax.Builder.Node     as NodeBuilder
-- import qualified Old.Luna.Syntax.Builder.Star     as StarBuilder
-- import qualified Old.Luna.Syntax.Builder.Symbol   as SymbolBuilder
-- import           Old.Luna.Syntax.Layer.Labeled    (label)
-- import           Old.Luna.Syntax.Repr.Graph
-- import           Old.Luna.Syntax.Symbol.Map       (SymbolMap)
-- import qualified Old.Luna.Syntax.Symbol.Map       as Symbol
-- import           Old.Luna.Syntax.Symbol.Network   (Network)

-- -- ====================================

-- typed a t = StarBuilder.with (const $ Just t) a

-- renderAndOpen lst = do
--     flip mapM_ lst $ \(name, g) -> render name $ toGraphViz g
--     open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)
-- -- ====================================


-- sampleGraph :: ((Ref Node, SymbolMap Label (Ref Edge)), Network Label)
-- sampleGraph = runIdentity
--       $ flip StarBuilder.evalT Nothing
--       $ flip Builder.runT def
--       $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
--       $ do
--             nameInt  <- _string "Int"
--             consInt  <- cons nameInt
--             i2 <- _int 2 `typed` consInt
--             i3 <- _int 3 `typed` consInt
--             namePlus <- _string "+"
--             accPlus  <- accessor namePlus i2
--             arr      <- arrow [consInt, consInt] Map.empty consInt
--             appPlus  <- app accPlus [arg i2, arg i3] `typed` arr

--             nameFloat <- _string "Float"
--             consFloat <- cons nameFloat
--             nameInt   <- _string "Int"
--             consInt   <- cons nameInt
--             arr1      <- readRef =<< arrow [consInt  , consInt  ] Map.empty consInt
--             arr2      <- readRef =<< arrow [consInt  , consFloat] Map.empty consInt
--             arr3      <- readRef =<< arrow [consInt  , consInt  ] Map.empty consFloat
--             arr4      <- readRef =<< arrow [consFloat, consInt  ] Map.empty consInt
--             arr5      <- readRef =<< arrow [consFloat, consFloat] Map.empty consInt

--             let mkItem arr = Symbol.PartiallySpecializedNetwork def $ Map.fromList $ map (\a -> (Symbol.fromArrow' $ matchArrow $ (uncoat a), def)) arr
--             let sm = Map.fromList $ [("Int.+", mkItem ([arr1, arr2, arr3, arr4, arr5] ))]

--             return (appPlus, sm)


-- mkSymbolMap :: Arrow t -> SymbolMap Label t
-- mkSymbolMap arr = Map.fromList [("Int.+", Symbol.PartiallySpecializedNetwork def $ Map.singleton (Symbol.fromArrow' arr) def )]



-- runGraph gr sm = runIdentityT
--             . flip SymbolBuilder.evalT sm
--             . flip StarBuilder.evalT Nothing
--             . flip Builder.runT gr
--             . flip NodeBuilder.evalT (Ref $ Node (0 :: Int))


-- evaluateTest :: Ref Node -> SymbolMap Label (Ref Edge) -> Network Label -> IO ((), Network Label)
-- evaluateTest i sm gr = Session.run $ runGraph gr sm $  do
--     Right r <-  runExceptT $   NodeRunner.runNode i
--     putStrLn "RESULT IS:"
--     print (Session.unsafeCast r :: Int)


-- main :: IO ()
-- main = do
--     let ((i, sm), g) = sampleGraph
--     -- let (lmap, gs) = addStdLiterals g
--     -- let (unis, g2) = pass2 lmap gs
--     -- let (_   , g3) = pass3 lmap unis g2

--     -- renderAndOpen [ ("g" , g)
--     --             --   , ("gs", gs)
--     --             --   , ("g2", g2)
--     --             --   , ("g3", g3)
--     --               ]
--     -- renderAndOpen [ ("g" , g)]
--     pprint g
--     _ <- evaluateTest i sm g
--     putStrLn "end"

-- matchArrow arr = case' arr $ match $ \a@(Arrow {}) -> a
