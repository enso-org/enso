{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Prelude.Luna                                    hiding (Num)

import           Control.Monad                                   (forM_)
import           Data.Construction
import           Data.Prop
import           Data.Record                                     hiding (cons)
import           Data.Version.Semantic                           (showVersion, version)
import           Type.Inference

import           Data.Graph
import           Data.Graph.Builder
import qualified Luna.Compilation.Env.Class                      as Env
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import           Luna.Syntax.AST.Term                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target)
import           Luna.Syntax.AST.Term                            hiding (source)
import qualified Luna.Syntax.AST.Term                            as Term
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Node          (NodeInferable, TermNode)
import           Luna.Syntax.Model.Network.Builder.Node.Class    (arg)
import           Luna.Syntax.Model.Network.Builder.Node.Inferred
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, runNetworkBuilderT)
import           Luna.Syntax.Model.Network.Class                 ()
import           Luna.Syntax.Model.Network.Term

import           Luna.Compilation.Pass.Interpreter.Layer         (InterpreterData (..), InterpreterLayer)
import qualified Luna.Compilation.Pass.Interpreter.Layer         as Layer
import           Luna.Compilation.Pass.Interpreter.Interpreter   as Interpreter

import           Data.Graph.Backend.VectorGraph
import qualified Data.Graph.Builder.Class                        as Graph

import           Control.Monad.Catch         (MonadCatch, MonadMask, catchAll)


graph1 :: forall term node edge nr er ls m n e c. ( term ~ Draft Static
                                                  , node ~ (ls :<: term)
                                                  , edge ~ Link (ls :<: term)
                                                  , nr   ~ Ref Node node
                                                  , er   ~ Ref Edge edge
                                                  , BiCastable     n (ls :<: term)
                                                  , BiCastable     e edge
                                                  , MonadIO       m
                                                  , NodeInferable m (ls :<: term)
                                                  , TermNode Star m (ls :<: term)
                                                  , TermNode Var  m (ls :<: term)
                                                  , TermNode Num  m (ls :<: term)
                                                  , TermNode Str  m (ls :<: term)
                                                  , TermNode Acc  m (ls :<: term)
                                                  , TermNode App  m (ls :<: term)
                                                  , HasProp InterpreterData (ls :<: term)
                                                  , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer
                                                  , Graph.MonadBuilder (Hetero (VectorGraph n e c)) m
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

graph2 :: forall term node edge nr er ls m n e c. ( term ~ Draft Static
                                                  , node ~ (ls :<: term)
                                                  , edge ~ Link (ls :<: term)
                                                  , nr   ~ Ref Node node
                                                  , er   ~ Ref Edge edge
                                                  , BiCastable      n (ls :<: term)
                                                  , BiCastable      e edge
                                                  , MonadIO         m
                                                  , NodeInferable   m (ls :<: term)
                                                  , TermNode Star   m (ls :<: term)
                                                  , TermNode Var    m (ls :<: term)
                                                  , TermNode Num    m (ls :<: term)
                                                  , TermNode Str    m (ls :<: term)
                                                  , TermNode Acc    m (ls :<: term)
                                                  , TermNode App    m (ls :<: term)
                                                  , TermNode Native m (ls :<: term)
                                                  , HasProp InterpreterData (ls :<: term)
                                                  , Prop    InterpreterData (ls :<: term) ~ InterpreterLayer
                                                  , Graph.MonadBuilder (Hetero (VectorGraph n e c)) m
                                                  )
       => m ([nr])
graph2 = do
    i1 <- int 2
    i2 <- int 3
    i3 <- int 4
    -- s1 <- str "abc"
    -- s2 <- str "def"
    -- s3 <- str "ghi"

    appPlus1   <- native (fromString "(+)") [i1, i2]

    appPlus2   <- native (fromString "(+)") [appPlus1, i3]

    let refsToEval = [appPlus1]

    forM_ refsToEval (\ref -> do
            (nd :: (ls :<: term)) <- read ref
            write ref (nd & prop InterpreterData . Layer.required .~ True)
        )

    return refsToEval


prebuild :: Show a => IO (Ref Node (NetLayers a :<: Draft Static), NetGraph a)
prebuild = runBuild def star

runBuild (g :: NetGraph a) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers a :<: Draft Static)))
                             $ runNetworkBuilderT g m

evalBuild = fmap snd ∘∘ runBuild


main :: IO ()
main = do
    putStrLn "Interpreter test"
    (_,  g00 :: NetGraph ()) <- prebuild
    flip catchAll (\e -> return ()) $ flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v
        flip catchAll (\e -> return ()) $ TypeCheck.runT $ do
            (refsToEval, g01) <- runBuild  g00 graph2
            g02               <- evalBuild g01 $ Interpreter.run refsToEval
            renderAndOpen [ ("g2", "g2", g02)
                          ]
    putStrLn "done"





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
-- import           Data.Layer.Coat
-- import qualified Data.Map                     as Map
-- import           Data.Variants                hiding (cons)
-- import           Data.Vector.Mutable          ()
-- import           Debug.Trace
-- import           Luna.Diagnostic.AST          as Diag (open, render, toGraphViz)
-- import           Luna.Interpreter.Label       (Label)
-- import qualified Luna.Interpreter.NodeRunner  as NodeRunner
-- import qualified Luna.Interpreter.Session     as Session
-- import           Luna.Syntax.AST.Term
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
