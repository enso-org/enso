{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}

module InterpreterSpec (spec) where

import           Prelude.Luna                                    hiding (Num)

import           Control.Monad                                   (forM_)
import           Data.Construction
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Data.Version.Semantic                           (showVersion, version)
import           Type.Inference

import           Data.Graph
import           Data.Graph.Builder
import qualified Luna.Config.Env                      as Env
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Pretty.GraphViz
import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                         (Dynamic, Static)
import           Old.Luna.Syntax.Term.Class                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import           Old.Luna.Syntax.Term.Class                            hiding (source)
import qualified Old.Luna.Syntax.Term.Class                            as Term
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (Sign (..))
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT, TermBuilder)
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term

import           Luna.Interpreter.Layer         (InterpreterData (..), InterpreterLayer)
import qualified Luna.Interpreter.Layer         as Layer
import qualified Luna.Interpreter.Value         as Value
import qualified Luna.Interpreter.Interpreter   as Interpreter

import qualified Data.Graph.Backend.NEC                          as NEC
import qualified Data.Graph.Builder.Class                        as Graph
import qualified Old.Luna.Syntax.Term.Expr.Lit                        as Lit

import           Control.Monad.Catch         (MonadCatch, MonadMask, catchAll)

import           Text.Printf                                     (printf)
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Inference.Unification     (StrictUnificationPass (..))
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import qualified Control.Monad.Writer                            as Writer
import qualified StdLibMock                                      as StdLib
import qualified Luna.Library.Symbol                             as Symbol
import           Control.Monad.Event                             (Dispatcher)

import           Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
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
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    s1 <- str "abc"
    s2 <- str "def"
    s3 <- str "ghi"

    j1 <- int 6
    j2 <- int 7
    plus <- str "+"


    appPlus0   <- app plus [arg j1, arg j2]

    accPlus1a  <- acc "+" i1
    appPlus1a  <- app accPlus1a [arg i2]

    accPlus1b  <- acc "+" i3
    appPlus1b  <- app accPlus1b [arg appPlus1a]

    accConc1a  <- acc "++" s2
    appConc1a  <- app accConc1a [arg s1]

    accConc1b  <- acc "++" appConc1a
    appConc1b  <- app accConc1b [arg s3]

    accLen     <- acc "len" appConc1b
    appLen     <- app accLen []

    accPlus2   <- acc "+" appPlus1b
    appPlus2   <- app accPlus2 [arg appLen]

    -- let refsToEval = [appConc1b, appPlus1a]
    let refsToEval = [appPlus0]

    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return refsToEval

-- graph2 :: forall term node edge nr er ls m n e c. ( term ~ Draft Static
--                                                   , node ~ (ls :<: term)
--                                                   , edge ~ Link (ls :<: term)
--                                                   , nr   ~ Ref Node node
--                                                   , er   ~ Ref Edge edge
--                                                   , BiCastable            n (ls :<: term)
--                                                   , BiCastable            e edge
--                                                   , MonadIO               m
--                                                   , NodeInferable         m (ls :<: term)
--                                                   , TermNode Lit.Star     m (ls :<: term)
--                                                   , TermNode Lit.Number   m (ls :<: term)
--                                                   , TermNode Lit.String   m (ls :<: term)
--                                                   , TermNode Var          m (ls :<: term)
--                                                   , TermNode Acc          m (ls :<: term)
--                                                   , TermNode App          m (ls :<: term)
--                                                   , TermNode Native       m (ls :<: term)
--                                                   , HasProp InterpreterData (ls :<: term)
--                                                   , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer
--                                                   , Graph.MonadBuilder (Hetero (VectorGraph n e c)) m
--                                                   )
--        => m ([nr])
graph2 = do
    i1 <- int 7
    i2 <- int 8
    i3 <- int 9
    -- s1 <- str "abc"
    -- s2 <- str "def"
    -- s3 <- str "ghi"

    -- appPlus1   <- native (fromString "(+)") [i1, i2]
    -- appPlus2   <- native (fromString "(+)") [appPlus1, i3]

    appPlus1   <- native (fromString "(+)")
    appPlus2   <- native (fromString "(+)")

    let refsToEval = [appPlus1]

    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return refsToEval


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


graph3 = do
    i1 <- int 1
    i2 <- int 2
    s1 <- str "hi"
    id <- var "id"
    apl <- acc "+" i1
    apppl <- app apl [arg i2]
    appid <- app id  [arg s1]

    -- let refsToEval = [i1]
    -- let refsToEval = [i1, appid]
    let refsToEval = [apppl, appid]


    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return ([apppl, appid], refsToEval)

graph4, graph5 :: forall term node edge nr er ls m n e c. ( term ~ Draft Static
                                                  , node ~ (ls :<: term)
                                                  , edge ~ Link (ls :<: term)
                                                  , nr   ~ Ref Node node
                                                  , er   ~ Ref Edge edge
                                                  , BiCastable            n (ls :<: term)
                                                  , BiCastable            e edge
                                                  , MonadIO               m
                                                  , NodeInferable         m (ls :<: term)
                                                  , TermNode Lit.Star     m (ls :<: term)
                                                  , TermNode Lit.Number   m (ls :<: term)
                                                  , TermNode Lit.String   m (ls :<: term)
                                                  , TermNode Var          m (ls :<: term)
                                                  , TermNode Acc          m (ls :<: term)
                                                  , TermNode App          m (ls :<: term)
                                                  , TermNode Native       m (ls :<: term)
                                                  , HasProp InterpreterData (ls :<: term)
                                                  , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer
                                                  , Graph.MonadBuilder (Hetero (NEC.Graph n e c)) m
                                                  )
       => m ([nr], [nr])

graph4 = do
    i1 <- int 2
    i2 <- int 3
    fun1 <- var "succ"

    act <- acc "times" i1
    apt <- app act [arg i2]

    ach <- acc "head" apt
    aph <- app ach []

    let refsToEval = [aph]

    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return ([aph], refsToEval)

graph5 = do
    iLen <- int 10
    iVal <- int 3
    fun  <- var "succ"

    act <- acc "times" iLen
    apt <- app act [arg iVal]

    acm <- acc "map" apt
    apm <- app acm [arg fun]

    ach <- acc "head" apm
    aph <- app ach []

    let refsToEval = [aph]

    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return ([aph], refsToEval)

graph6 = do
    iLen  <- int 5
    iVal  <- int 3
    iInit <- int 1
    fun   <- var "(+)"

    act <- acc "times" iLen
    apt <- app act [arg iVal]

    acf <- acc "fold" apt
    apf <- app acf [arg iInit, arg fun]

    let refsToEval = [apf]

    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return ([apf], refsToEval)


collectGraph tag = do
    putStrLn $ "after pass: " ++ tag
    tcState <- TypeCheckState.get
    putStrLn $ "TCState is: " ++ show tcState
    g <- Graph.get
    Writer.tell [(tag, g)]

seq3 a b c = Sequence a $ Sequence b c
seq4 a b c d = Sequence a $ seq3 b c d

test2 :: IO ()
test2 = do
    (_, g :: NetGraph) <- prebuild

    let graph' = do
            two <- int 2
            two' <- int 2
            a <- var "app"
            bl <- blank
            accpl <- acc "+" bl
            bl2 <- blank
            apl <- app accpl [arg bl2, arg two]
            apl' <- app a [arg apl, arg two']
            forM_ [apl'] (\ref -> do
                    (nd :: (ls :<: term)) <- read ref
                    write ref (nd & prop InterpreterData . Layer.required .~ True)
                )
            return [apl']

    res <- flip Env.evalT def $ do
        TypeCheck.runT $ do
            ([root], gb) <- runBuild g graph'

            (gs, gtc) <- runBuild gb $ do
                Symbol.loadFunctions StdLib.symbols
                TypeCheckState.modify_ $ TypeCheckState.freshRoots .~ [root]
                -- collectGraph "Initial"
                let tc = (seq4
                             ScanPass
                             LiteralsPass
                             StructuralInferencePass
                             (Loop $ Sequence
                                 (Loop $ seq4
                                     SymbolImportingPass
                                     (Loop $ StrictUnificationPass Positive False)
                                     FunctionCallingPass
                                     (Loop $ StrictUnificationPass Positive False))
                                 (StrictUnificationPass Negative True)))
                TypeCheck.runTCPass tc

            gint <- intRun gtc [root]

            (dupa, gint') <- runBuild gint $ do
                follow (prop InterpreterData . Layer.value) root

            return $ dupa ^?! _Right

    Value.unsafeFromData <$> Value.toIO res `shouldReturn` (4::Int)

test1 :: IO ()
test1 = do
    (_,  g :: NetGraph) <- prebuild

    -- Running compiler environment
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v

        -- Running Type Checking compiler stage
        (gs, gint) <- TypeCheck.runT $ do
            ((roots, refsToEval), gb) <- runBuild g graph6

            (gs, gtc) <- runBuild gb $ Writer.execWriterT $ do
                Symbol.loadFunctions StdLib.symbols
                TypeCheckState.modify_ $ (TypeCheckState.freshRoots .~ roots)
                collectGraph "Initial"
                let tc = (seq4
                             ScanPass
                             LiteralsPass
                             StructuralInferencePass
                             (Loop $ Sequence
                                 (Loop $ seq4
                                     SymbolImportingPass
                                     (Loop $ StrictUnificationPass Positive False)
                                     FunctionCallingPass
                                     (Loop $ StrictUnificationPass Positive False))
                                 (StrictUnificationPass Negative True)))

                TypeCheck.runTCWithArtifacts tc collectGraph

            -- gint <- evalBuild gtc $ Interpreter.run refsToEval
            gint <- intRun gtc refsToEval
            return (gs, gint)

        let names = printf "%02d" <$> ([0..] :: [Int])
        let graphs = zipWith (\ord (tag, g) -> (ord, ord <> "_" <> tag, g)) names gs
        putStrLn $ intercalate " " $ (view _2) <$> graphs
        -- renderAndOpen [ ("gint", "gint", gint) ]
        -- renderAndOpen [ last graphs ]
        -- renderAndOpen graphs
    print "end"


intRun :: ( MonadFix m
          , MonadMask m
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
    describe "interpreter" $ do
        it "interprets" $
            silence test1
        it "2+2" $
            test2

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
-- import           Luna.Interpreter.Label       (Label)
-- import qualified Luna.Interpreter.NodeRunner  as NodeRunner
-- import qualified Luna.Interpreter.Session     as Session
-- import           Old.Luna.Syntax.Term.Class
-- import           Luna.Syntax.Builder
-- import qualified Luna.Syntax.Builder          as Builder
-- import qualified Luna.Syntax.Builder.Node     as NodeBuilder
-- import qualified Luna.Syntax.Builder.Star     as StarBuilder
-- import qualified Luna.Syntax.Builder.Symbol   as SymbolBuilder
-- import           Luna.Syntax.Layer.Labeled    (label)
-- import           Luna.Syntax.Repr.Graph
-- import           Luna.Syntax.Symbol.Map       (SymbolMap)
-- import qualified Luna.Syntax.Symbol.Map       as Symbol
-- import           Luna.Syntax.Symbol.Network   (Network)

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
