{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE RankNTypes                #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds     #-}

module Main where

import           Data.Graph
import           Data.Graph.Builders
import           Prologue                                        hiding (Version, cons, read, ( # ), Num, Cons)

import           Text.Printf                                     (printf)
import qualified Control.Monad.Writer                            as Writer
import           Control.Monad.Event
import           Data.Attr                                       (attr)
import           Data.Construction
import           Data.Container                                  (elems, index_)
import           Data.Container                                  hiding (impossible)
import           Data.Graph.Builder
import           Data.Graph.Query                                hiding (Graph)
import qualified Data.Graph.Query                                as Sort
import           Data.Index                                      (idx)
import           Data.Layer.Cover
import qualified Data.Map                                        as Map
import           Data.Prop
import           Data.Record                                     hiding (Layout, cons)
import           Data.Version.Semantic
import           Development.Placeholders
import           Type.Inference

import qualified Luna.Compilation.Env.Class                      as Env
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Inference.Unification     (UnificationPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import           Luna.Diagnostic.Vis.GraphViz
import           Luna.Evaluation.Runtime                         (Dynamic, Static)
import qualified Luna.Evaluation.Runtime                         as Runtime
import qualified Luna.Evaluation.Model                           as EvalModel
import qualified Luna.Library.Standard                           as StdLib
import qualified Luna.Library.Symbol.Class                       as Symbol
import           Luna.Syntax.AST.Term                            hiding (Draft, Expr, Lit, Source, Target, Thunk, Val, source, target, Input)
import qualified Luna.Syntax.AST.Term                            as Term
import           Data.Graph.Builder.Ref                          as Ref
import qualified Data.Graph.Builder.Class                        as Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetGraph, NetLayers, NetCluster, runNetworkBuilderT, fmapInputs, inputstmp)
import           Luna.Syntax.Model.Network.Class                 (Network)
import           Luna.Syntax.Model.Network.Term

import Data.Graph.Backend.VectorGraph

title s = putStrLn $ "\n" <> "-- " <> s <> " --"


--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast (Edge e) = cast e
--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast e = Edge $ cast e
-- --------------------------------------
--  !!! KEEP THIS ON THE BEGINNING !!! --
-- --------------------------------------
-- - vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ---
prebuild :: Show a => IO (Ref Node (NetLayers a :<: Draft Static), NetGraph a)
prebuild = runBuild def star

prebuild2 :: Show a => IO (NetLayers a :<: Draft Static, NetGraph a)
prebuild2 = runBuild def  (read =<< star)


runBuild (g :: NetGraph a) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers a :<: Draft Static)))
                             $ runNetworkBuilderT g m

evalBuild = fmap snd ∘∘ runBuild


input_g1 :: ( term ~ Draft Static
            , nr   ~ Ref Node (ls :<: term)
            , MonadIO       m
            , NodeInferable m (ls :<: term)
            , TermNode Star m (ls :<: term)
            , TermNode Var  m (ls :<: term)
            , TermNode App  m (ls :<: term)
            , TermNode Acc  m (ls :<: term)
            ) => m ([nr],[nr],[nr])
input_g1 = do
    f  <- var' "f"
    a  <- var' "a"
    b  <- var' "b"
    r1 <- app' f [arg a, arg b]

    x  <- acc' "x" r1

    g  <- var' "g"
    r2 <- app' g [arg x]
    return ([r1,r2], [x], [f,g])

input_g2 :: ( term ~ Draft Static
            , nr   ~ Ref Node (ls :<: term)
            , MonadIO       m
            , NodeInferable m (ls :<: term)
            , TermNode Star m (ls :<: term)
            , TermNode Var  m (ls :<: term)
            , TermNode App  m (ls :<: term)
            , TermNode Acc  m (ls :<: term)
            , TermNode Num  m (ls :<: term)
            ) => m ([nr],[nr],[nr],[nr])
input_g2 = do
    -- The expression here is `(id (1.+ (id 2)).toString).length`
    n1  <- int 1
    n2  <- int 2
    pl  <- acc "+" n1
    id1 <- var "id"
    --id2 <- var "id"
    apid2 <- app id1 [arg n2]
    br  <- app pl [arg apid2]
    ts  <- acc "toString" br
    tsa <- app ts []
    idtsa <- app id1 [arg tsa]
    le  <- acc "length" idtsa
    lea <- app le []

    return ([n1, n2], [br, tsa, lea, apid2, idtsa], [pl, ts, le], [pl, ts, le, id1])

input_g3 :: ( term ~ Draft Static
            , nr   ~ Ref Node (ls :<: term)
            , MonadIO       m
            , NodeInferable m (ls :<: term)
            , TermNode Star m (ls :<: term)
            , TermNode Var  m (ls :<: term)
            , TermNode App  m (ls :<: term)
            , TermNode Acc  m (ls :<: term)
            , TermNode Num  m (ls :<: term)
            , TermNode Str  m (ls :<: term)
            ) => m ([nr],[nr],[nr],[nr])
input_g3 = do
    -- The expression here is `(id (1.+ (id 2)).toString).length`
    n1  <- int 1
    s1  <- str "Str!"
    foo <- var "foo"
    a1  <- app foo [arg n1]
    a2  <- app foo [arg s1]

    --n2  <- int 2
    --bar <- var "bar"
    --a3  <- app bar [arg n2]

    return ([n1, s1], [a1, a2], [], [foo])
    --return ([n1, s1, n2], [a1, a2, a3], [], [foo, bar])

input_simple1 :: ( term ~ Draft Static
                 , nr   ~ Ref Node (ls :<: term)
                 , MonadIO       m
                 , NodeInferable m (ls :<: term)
                 , TermNode Star m (ls :<: term)
                 , TermNode Var  m (ls :<: term)
                 , TermNode App  m (ls :<: term)
                 , TermNode Acc  m (ls :<: term)
                 , TermNode Num  m (ls :<: term)
                 ) => m ([nr],[nr],[nr],[nr])
input_simple1 = do
    -- The expression here is `(id (1.+ (id 2)).toString).length`
    n1  <- int 1
    n2  <- int 2
    pl  <- acc "+" n1
    br  <- app pl [arg n2]

    return ([n1, n2], [br], [pl], [pl])


input_simple2 :: ( term ~ Draft Static
            , nr   ~ Ref Node (ls :<: term)
            , MonadIO       m
            , NodeInferable m (ls :<: term)
            , TermNode Star m (ls :<: term)
            , TermNode Var  m (ls :<: term)
            , TermNode App  m (ls :<: term)
            , TermNode Acc  m (ls :<: term)
            , TermNode Num  m (ls :<: term)
            , TermNode Str  m (ls :<: term)
            ) => m ([nr],[nr],[nr],[nr])
input_simple2 = do
    -- The expression here is `(id (1.+ (id 2)).toString).length`
    n1  <- int 1
    s1  <- str "s"
    fid <- var "id"
    appIdN <- app fid [arg n1]
    appIdS <- app fid [arg s1]

    return ([n1, s1], [appIdN, appIdS], [], [fid])


input_g1_resolution_mock :: ( term ~ Draft Static
                            , node ~ (ls :<: term)
                            , edge ~ Link (ls :<: term)
                            , nr   ~ Ref Node node
                            , er   ~ Ref Edge edge
                            , BiCastable     n (ls :<: term)
                            , BiCastable     e edge
                            , MonadIO        m
                            , NodeInferable  m (ls :<: term)
                            , TermNode Star  m (ls :<: term)
                            , TermNode Var   m (ls :<: term)
                            , TermNode App   m (ls :<: term)
                            , TermNode Acc   m (ls :<: term)
                            , TermNode Cons  m (ls :<: term)
                            , TermNode Lam   m (ls :<: term)
                            , TermNode Unify m (ls :<: term)
                            , HasProp Type (ls :<: term)
                            , Prop    Type (ls :<: term) ~ er
                            , Graph.MonadBuilder (Hetero (VectorGraph n e c)) m
                            , Castable e edge
                            ) => [nr] -> m [nr]
input_g1_resolution_mock [f,g] = do
    c_int   <- cons' "Int"
    c_str   <- cons' "String"
    ft_mock <- lam' [arg c_int, arg c_int] c_int
    gt_mock <- lam' [arg c_str]            c_str
    f'      <- read f
    g'      <- read g
    let ft_c = f' # Type
        gt_c = g' # Type
    ft      <- follow source ft_c
    gt      <- follow source gt_c
    ft_u    <- unify' ft ft_mock
    gt_u    <- unify' gt gt_mock
    return [ft_u, gt_u]

--input_g2 :: ( ls   ~ NetLayers ()
--            , term ~ Draft Static
--            , nr   ~ Ref (Node $ (ls :<: term))
--            , MonadIO       m
--            , NodeInferable m (ls :<: term)
--            , TermNode Star m (ls :<: term)
--            , TermNode Var  m (ls :<: term)
--            , TermNode Num  m (ls :<: term)
--            , TermNode Str  m (ls :<: term)
--            , TermNode Acc  m (ls :<: term)
--            , TermNode App  m (ls :<: term)
--            )
--         => m (nr, ([nr], [nr]))
--input_g2 = do
--    i1 <- int 2
--    i2 <- int 3
--    i3 <- int 4
--    s1 <- str "abc"
--    s2 <- str "def"
--    s3 <- str "ghi"

--    accPlus1a  <- acc "+" i1
--    appPlus1a  <- app accPlus1a [arg i2]

--    accPlus1b  <- acc "+" i3
--    appPlus1b  <- app accPlus1b [arg appPlus1a]

--    accConc1a  <- acc "++" s2
--    appConc1a  <- app accConc1a [arg s1]

--    accConc1b  <- acc "++" appConc1a
--    appConc1b  <- app accConc1b [arg s3]

--    accLen     <- acc "len" appConc1b
--    appLen     <- app accLen []

--    accPlus2   <- acc "+" appPlus1b
--    appPlus2   <- app accPlus2 [arg appLen]

--    return ( appPlus2
--           , ( [appPlus1a, appPlus1b, appConc1a, appConc1b, appLen, appPlus2]
--             , [accPlus1a, accPlus1b, accConc1a, accConc1b, accLen, accPlus2]
--             )
--           )


symbolMapTest :: IO ()
symbolMapTest = do
    title "Symbol map testing"
    (_, g :: NetGraph ()) <- prebuild
    ((plus, sin, err, l1, l2), (g :: NetGraph ())) <- runBuild g $ do
        i1 <- int 1
        i2 <- int 2
        s1 <- str "hello world!"
        s2 <- str "also hello!"
        s3 <- str "yo yo guyz!"
        tint <- cons "Int"
        tstr <- cons "String"
        reconnect i1 (prop Type) tint
        reconnect i2 (prop Type) tint
        reconnect s1 (prop Type) tstr
        reconnect s2 (prop Type) tstr
        reconnect s3 (prop Type) tstr
        plus <- acc "+" i1
        err  <- acc "noMethod" s1
        l1   <- acc "length" s2
        l2   <- acc "length" s3
        sin  <- var "sin"
        {-reconnect s1 (prop Redirect) err-}
        {-write s1 $ s & prop Redirect .~ t-}

        return (plus, sin, err, l1, l2)

    renderAndOpen [("beforeImporting", "beforeImporting", g)]

    (f, (g :: NetGraph ())) <- flip Symbol.evalT def $ runBuild g $ do
        Symbol.loadFunctions StdLib.symbols
        mapM Importing.processNode [plus, sin, err, l1, l2]

    mapM print f

    renderAndOpen [("afterImporting", "afterImporting", g)]
    return ()

collectGraph tag = do
    g <- Graph.get
    Writer.tell [(tag, g)]

test1 :: IO ()
test1 = do
    (_,  g :: NetGraph () ) <- prebuild


    -- Running compiler environment
    flip Env.evalT def $ do
        v <- view version <$> Env.get
        putStrLn $ "Luna compiler version " <> showVersion v

        -- Running Type Checking compiler stage
        (gs, _) <- TypeCheck.runT $ runBuild g $ Writer.execWriterT $ do
            --(lits, apps, accs, funcs) <- input_g2
            --(lits, apps, accs, funcs) <- input_simple1
            (lits, apps, accs, funcs) <- input_simple2
            collectGraph "Initial"

            Symbol.loadFunctions StdLib.symbols
            TypeCheckState.modify_ $ (TypeCheckState.untypedApps       .~ apps )
                                   . (TypeCheckState.untypedAccs       .~ accs )
                                   . (TypeCheckState.untypedLits       .~ lits )
                                   . (TypeCheckState.unresolvedSymbols .~ funcs)

            let tc = Sequence LiteralsPass
                   $ Sequence StructuralInferencePass
                   $ Loop $ Sequence (Loop UnificationPass)
                          $ SymbolImportingPass

            TypeCheck.runTCWithArtifacts tc collectGraph

        let names = printf "%02d" <$> ([0..] :: [Int])
        renderAndOpen $ zipWith (\ord (tag, g) -> (ord, ord <> "_" <> tag, g)) names gs
    print "end"






data Input  = Input  deriving (Show, Eq, Ord)
data Output = Output deriving (Show, Eq, Ord)

data Consistency node edge = Consistent
                           | Inconsistent (Error node edge)

data Error node edge = MissingInput  node (node # Input )
                     | MissingOutput node (node # Output)
                     | MissingSource edge (edge # Source)
                     | MissingTarget edge (edge # Target)
                     | BrokenInput   node (node # Input )
                     | BrokenOutput  node (node # Output)
                     | BrokenSource  edge (edge # Source)
                     | BrokenTarget  edge (edge # Target)



newtype Network' ls cls = Network' (Hetero (VectorGraph (ls :<: Raw) (Link (ls :<: Raw)) (cls :< SubGraph (ls :<: Raw))))
makeWrapped ''Network'

type instance Prop Node (Network' ls cls) = ls :<: Draft Static
type instance Prop Edge (Network' ls cls) = Link (ls :<: Draft Static)



class Referenced r t a where
    refs :: t -> [Ref r a]

type Referenced' r t = Referenced r t (t # r)

refs' :: Referenced' r t => t -> [Ref r (t # r)]
refs' = refs

ref_nodes :: Referenced Node t a => t -> [Ref Node a]
ref_nodes = refs

ref_nodes' :: Referenced' Node t => t -> [Ref Node (t # Node)]
ref_nodes' = refs'

ref_edges' :: Referenced' Edge t => t -> [Ref Edge (t # Edge)]
ref_edges' = refs'

--inputs - combination of type and args

tp :: Selector Single Type
tp = Selector

class KnownGraph2 g where
    ref_nodes2' :: g -> [Ref Node (g # Node)]
    ref_edges2' :: g -> [Ref Edge (g # Edge)]

instance ( Referenced' Node g
         , Referenced' Edge g
         ) => KnownGraph2 g where
    ref_nodes2' = ref_nodes' ; {-# INLINE ref_nodes2' #-}
    ref_edges2' = ref_edges' ; {-# INLINE ref_edges2' #-}

instance KnownGraph2 I where ref_nodes2' = impossible

--instance UnwrappedGetter sel p (Node a)  => Getter2 sel p (Node a)  where getter2 s = getter2 s ∘ unwrap'
--instance UnwrappedGetter sel p (ls :<: t) => Getter2 sel p (ls :<: t) where getter2 s = getter2 s ∘ unwrap'


check :: (MonadIO m, KnownGraph2 g) => g -> m ()
check g = do
    let nrs = ref_nodes2' g
    print nrs
    return ()


nodes :: Selector Every Node
nodes = Selector

--a = nodes

main2 :: IO ()
main2 = do
    (nr, g_ :: NetGraph ()) <- prebuild
    let g = Network' g_
    --print $ (refs g :: [Ref Node (NetLayers () :<: Draft Static)])
    check g
    return ()

--instance Getter2 Every Node (NetGraph ()) where
--    getter2 _ g = undefined where
        --g' = unwrap' g :: _

--instance Referenced Node (Network' ls) (ls :<: Draft Static) where refs = fmap Ref ∘ usedIxes ∘ view (wrapped' ∘ wrapped' ∘ nodeGraph)
instance Referenced Node (Network' ls cls) (ls :<: Draft Static)        where refs = fmap retarget ∘ refs' ∘ unwrap'
instance Referenced Edge (Network' ls cls) (Link (ls :<: Draft Static)) where refs = fmap retarget ∘ refs' ∘ unwrap'

instance Referenced r t a => Referenced r (Hetero t) a where refs = refs ∘ unwrap'

instance n ~ n' => Referenced Node (VectorGraph n e c) n' where refs = fmap Ref ∘ usedIxes ∘ view nodeGraph
instance e ~ e' => Referenced Edge (VectorGraph n e c) e' where refs = fmap Ref ∘ usedIxes ∘ view edgeGraph

--g -> [Ref Node (g # Node)]

--Ref $ Node $ ls :<: term --> read
--      Node $ ls :<: term

--Ref $ Arc src tgt --> read
--      Arc src tgt


--Ref Node $ ls :<: term --> read
--         $ ls :<: term

--Ref Edge $ Arc src tgt --> read
--           Arc src tgt

--Ref Cluster $ Subgraph



main :: IO ()
main = do
    --showcase
    {-symbolMapTest-}
    test1
    --test2
    --main2
    return ()


--test2 :: IO ()
--test2 = do
--    (_,  g00 :: NetGraph ()) <- prebuild

--    -- Running compiler environment
--    flip Env.evalT def $ do
--        v <- view version <$> Env.get
--        putStrLn $ "Luna compiler version " <> showVersion v

--        -- Running Type Checking compiler stage
--        TypeCheck.runT $ do
--            ((root, (apps, accs)), g01) <- runBuild  g00 input_g2
--            (literals, g02)     <- runBuild  g01 $ LiteralsUtils.run root
--            g03                 <- evalBuild g02 $ LiteralsAssignement.run literals
--            (unis, g04)         <- runBuild  g03 $ StructInference.run apps accs
--            (gs05, g05)         <- runBuild  g04 $ Unification.run 1 unis
--            let gss = zipWith (,) (("g0" <>) ∘ show <$> [5..]) gs05
--            renderAndOpen $ [ ("g02", g02)
--                            , ("g03", g03)
--                            , ("g04", g04)
--                            --, ("g05", g05)
--                            ] <> gss
--    putStrLn "done"




-----------------------
-- === Showcase === ---
-----------------------

showcase :: IO ()
showcase = do
    (_,  g :: NetGraph () ) <- prebuild
    (_, g') <- foo g
    renderAndOpen [ ("g", "g", g')
                  ]

foo :: forall a. Show a => NetGraph a -> IO (Ref Node (NetLayers a :<: Draft Static), NetGraph a)
--foo :: NetGraph -> IO ((), NetGraph)
foo g = runNetworkBuilderT g
    $ do
    title "basic element building"
    s1 <- int 8
    s2 <- str "test"
    print s1

    title "reading node references"
    s1_v <- read s1
    s2_v <- read s2
    print s1_v

    title "manual connection builing"
    c1 <- connection s1 s2

    title "reading connection references"
    c1_v <- read c1
    print c1_v

    title "edge following"
    c1_tgt <- follow target c1
    when (c1_tgt /= s2) $ fail "reading is broken!"
    print "ok!"

    title "pattern matching"
    print $ uncover s1_v
    print $ caseTest (uncover s1_v) $ do
        match $ \Star -> "its a star! <3"
        match $ \ANY  -> "something else!"

    title "complex element building"
    u1 <- unify s1 s2
    print (u1 :: Ref Node (NetLayers a :<: Draft Static))
    u1_v <- read u1

    title "inputs reading"
    let u1_ins = u1_v # Inputs
    print u1_ins

    title "params reading"
    let s1t = s1_v # Type
        s1s = s1_v # Succs
    print s1t
    print s1s

    title "subgraph definition"
    sg1 :: Ref Cluster $ NetCluster a <- subgraph
    sg2 :: Ref Cluster $ NetCluster a <- subgraph
    include s1 sg1
    include s2 sg1
    include s2 sg2
    print "done"

    title "subgraph lookup"
    print =<< sg1 `includes` s2

    title "subgraph modification"
    exclude s2 sg1
    print =<< sg1 `includes` s2

    return s1


--fmapInputs :: OverElement (MonoTMap t) r => (t -> t) -> (r -> r)


--class TFunctor t r a a' | t r a -> a' where fmapT :: (t -> r) -> a -> a'



--class OverElement ctx rec where overElement :: Proxy ctx -> (forall v. ctx v => v -> v) -> rec -> rec


--instance (out ~ Str          ) => TFunctor2 t r Str           out where fmapT2 = undefined
--instance (out ~ Star         ) => TFunctor2 t r Star          out where fmapT2 = undefined
--instance (out ~ Num          ) => TFunctor2 t r Num           out where fmapT2 = undefined
--instance (out ~ (Lam      r')) => TFunctor2 t r (Lam      t') out where fmapT2 = undefined
--instance (out ~ (Acc    n r')) => TFunctor2 t r (Acc    n t') out where fmapT2 = undefined
--instance (out ~ (Native n r')) => TFunctor2 t r (Native n t') out where fmapT2 = undefined
--instance (out ~ (App      r')) => TFunctor2 t r (App      t') out where fmapT2 = undefined
--instance (out ~ (Unify    r')) => TFunctor2 t r (Unify    t') out where fmapT2 = undefined
--instance (out ~ (Var    n   )) => TFunctor2 t r (Var    n   ) out where fmapT2 = undefined
--instance (out ~ (Cons   n   )) => TFunctor2 t r (Cons   n   ) out where fmapT2 = undefined
--instance (out ~ Blank        ) => TFunctor2 t r Blank         out where fmapT2 = undefined







--instance (out ~ Str          ) => TFunctor2 t r Str           out where fmapT2 = undefined
--instance (out ~ Star         ) => TFunctor2 t r Star          out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r Num           out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Lam      t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Acc    n t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Native n t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (App      t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Unify    t') out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Var    n   ) out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r (Cons   n   ) out where fmapT2 = undefined
--instance ()                    => TFunctor2 t r Blank         out where fmapT2 = undefined

--fmaptmp :: forall layout term rt x.
--      (MapTryingElemList_
--                            (Elems term (ByRuntime rt Str x) x)
--                            (TFoldable x)
--                            (Term layout term rt), x ~ Layout layout term rt) => Term layout term rt -> [x]


--class TFunctor t r a a' | t r a -> a' where fmapT :: (t -> r) -> a -> a'

--class WithElement' ctx rec a where withElement' :: Proxy ctx -> (forall v. ctx v a => v -> a) -> rec -> a
--instance (MapTryingElemList els ctx rec a, els ~ Layout2 Variant (RecordOf rec)) => WithElement' ctx rec a where withElement' = mapTryingElemList (p :: P els)

----------------------------
-- === Sorting stuff === ---
----------------------------

--type instance Item (NetGraph a) = Ref $ Node (NetLayers a :<: Draft Static)

--instance Sort.CompleteGraph     (VectorGraph (NetLayers a :<: Raw) (Link (NetLayers a :<: Raw)))

--instance Sort.MarkableGraph     (VectorGraph (NetLayers a :<: Raw) (Link (NetLayers a :<: Raw))) where
--    markNode ref g = snd $ rebuildNetwork' g $ do
--        Ref.with ref $ prop Markable .~ True
--    isMarked ref g = fst $ rebuildNetwork' g $ do
--        node <- read ref
--        return $ node # Markable

--instance Sort.Graph             (VectorGraph (NetLayers a :<: Raw) (Link (NetLayers a :<: Raw))) where
--    listNodes g = Ref <$> (Ref <$> usedIxes $ $ g ^. nodeGraph)

--instance Sort.ForwardEdgedGraph (VectorGraph (NetLayers a :<: Raw) (Link (NetLayers a :<: Raw))) where
--    successors ref g = fst $ rebuildNetwork' g $ do
--        node <- read ref
--        mapM (follow target) $ node ^. prop Succs



-------------------------
-- === Benchmarks === ---
-------------------------


--data Bench a = Bench1 a
--             | Bench2
--             deriving (Show)

--main = do


--    args <- getArgs
--    let mode   = read (args !! 0) :: Int
--        argnum = read (args !! 1) :: Int
--        nums = [0..argnum]


--    case mode of
--        0 -> do
--            let ls = const star . show <$> nums
--                pattest l = caseTest l $ do
--                    variantMatch (\Star -> (1 :: Int))
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls
--        1 -> do
--            let ls = const Bench2 . show <$> nums
--                pattest l = case l of
--                    Bench2 -> (1 :: Int)
--                getnum _ = 0
--            print $ sum $ pattest <$> ls
--            --print $ sum $ getnum <$> ls


-- === Performance notes === ---
-- Performance drops observed:
--     - using custom State class and a wrapper for pattern-matches causes drop
--       probably because automatically derived methods in the State wrapper are not inlined (TBI).
--     - using the `reverse` function in pattern match causes a drop, but it should be computed always during the compile time.
