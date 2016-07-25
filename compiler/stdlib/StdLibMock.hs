{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StdLibMock where

import Prelude.Luna

import Data.Construction
import Data.Maybe                                   (maybeToList)
import Data.Prop
import Data.Graph
import Data.Graph.Builder
import Control.Monad.State                          as State

import Luna.Runtime.Dynamics                      (Static, Dynamic)
import Luna.Library.Symbol                    (MonadSymbol, SymbolMap)

import Luna.Syntax.Term.Function                     (Function (..), Signature (..))
import Old.Luna.Syntax.Term.Class
import Luna.Syntax.Model.Layer
import Luna.Syntax.Model.Network.Builder.Node
import Luna.Syntax.Model.Network.Builder.Term.Class (runNetworkBuilderT, NetworkBuilderT, NetLayers, NetGraph)
import Luna.Syntax.Model.Network.Class              ()
import Luna.Syntax.Model.Network.Term

import qualified Old.Luna.Syntax.Term.Expr.Lit         as Lit
import qualified Data.Map                         as Map
import qualified Luna.Syntax.Name.Path     as QualPath
import qualified Luna.Syntax.Term.Function         as Function

#define FunBuilderCtx(m) ( n ~ (NetLayers :<: Draft Static)      \
                         , nodeRef ~ Ref Node  n                 \
                         , TermNode Cons     m n                 \
                         , TermNode Blank    m n                 \
                         , TermNode Native   m n                 \
                         , TermNode Lit.Star m n                 \
                         , TermNode Lam      m n                 \
                         , TermNode Var      m n                 \
                         , TermNode App      m n                 \
                         , MonadFix m                            \
                         , NetworkBuilderT NetGraph m0 Identity  \
                         , MonadBuilder NetGraph m               \
                         , Monad m0                              \
                         , Destructor m (Ref Edge (Link n))      \
                         , m ~ StateT (Map.Map TPRep nodeRef) m0 \
                         )

data TPRep = TCons String [TPRep]
           | TVar  String
           | TLam         [TPRep] TPRep
           deriving (Show, Eq, Ord)

consOf :: String -> [TPRep] -> TPRep
consOf = TCons

consOfS :: String -> TPRep -> TPRep
consOfS cons t = TCons cons [t]

listOf :: TPRep -> TPRep
listOf = consOfS "List"

refOf :: TPRep -> TPRep
refOf = consOfS "Ref"

scons :: String -> TPRep
scons = flip TCons []

buildGraph :: forall m0 m n b nodeRef . FunBuilderCtx(m) => m b -> (b, NetGraph)
buildGraph m = runIdentity $ runNetworkBuilderT def $ evalStateT ((star :: m nodeRef) >> m) def

{-makeFunction :: FunBuilderCtx(m) => m (Signature nodeRef) -> Function (Ref Node n) NetGraph-}
makeFunction bldr = Function signature body where
    (signature, body) = buildGraph bldr

typed :: FunBuilderCtx(m) => m nodeRef -> nodeRef -> m nodeRef
typed b t = do
    el <- b
    reconnect (prop Type) el t
    return el

getTypeRep :: FunBuilderCtx(m) => TPRep -> m nodeRef
getTypeRep tp = do
    m <- State.get
    case Map.lookup tp m of
        Just r  -> return r
        Nothing -> do
            r <- case tp of
                TVar name     -> var . fromString $ name
                TCons n as    -> mapM (fmap arg . getTypeRep) as >>= cons (fromString n)
                TLam args res -> do
                    argNodes <- mapM getTypeRep args
                    resNode  <- getTypeRep res
                    lam (arg <$> argNodes) resNode
            State.modify $ Map.insert tp r
            return r

makeNativeFun :: FunBuilderCtx(m) => String -> Maybe TPRep -> [TPRep] -> TPRep -> (String, m (Signature nodeRef))
makeNativeFun name selfTypeRep argTypesRep outTypeRep = (,) name $ do
    selfType <- mapM getTypeRep selfTypeRep
    argTypes <- mapM getTypeRep argTypesRep
    outType  <- getTypeRep outTypeRep
    self     <- mapM (typed blank) selfType
    args     <- mapM (typed blank) argTypes
    let nativeArgTypes = maybeToList selfType <> argTypes
    let nativeArgs = maybeToList self <> args
    lambda   <- lam (arg <$> nativeArgTypes) outType
    native   <- native (fromString name) `typed` lambda
    out      <- app native (arg <$> nativeArgs) `typed` outType
    return $ Signature self (arg <$> args) out

makeId :: FunBuilderCtx(m) => m (Signature nodeRef)
makeId = do
    tpV   <- var "#idTp1"
    n     <- blank `typed` tpV
    return $ Signature Nothing [arg n] n

makeConst :: FunBuilderCtx(m) => m (Signature nodeRef)
makeConst = do
    tpVa <- var "#a"
    tpVb <- var "#b"
    na   <- blank `typed` tpVa
    nb   <- blank `typed` tpVb
    return $ Signature Nothing [arg na, arg nb] na

makeFlipConst :: FunBuilderCtx(m) => m (Signature nodeRef)
makeFlipConst = do
    tpVa <- var "#a"
    tpVb <- var "#b"
    na   <- blank `typed` tpVa
    nb   <- blank `typed` tpVb
    return $ Signature Nothing [arg na, arg nb] nb

makeFlipConst2 :: FunBuilderCtx(m) => m (Signature nodeRef)
makeFlipConst2 = do
    tpVa <- var "#a"
    tpVb <- var "#b"
    tpVc <- var "#c"
    na   <- blank `typed` tpVa
    nb   <- blank `typed` tpVb
    nc   <- blank `typed` tpVc
    return $ Signature Nothing [arg na, arg nb, arg nc] nc

symbols :: SymbolMap (NetLayers :<: Draft Static) NetGraph
symbols = Map.fromList $ fmap (\(n, b) -> (QualPath.mk (n :: String), makeFunction b)) symbolsList

symbolsList = [

------------------
-- === List === --
------------------

      makeNativeFun "List.+"        (Just $ listOf $ TVar "#a")    [listOf $ TVar "#a"]                                           (listOf $ TVar "#a")
    , makeNativeFun "List.append"   (Just $ listOf $ TVar "#a")    [TVar "#a"]                                                    (listOf $ TVar "#a")
    , makeNativeFun "List.prepend"  (Just $ listOf $ TVar "#a")    [TVar "#a"]                                                    (listOf $ TVar "#a")
    , makeNativeFun "List.length"   (Just $ listOf $ TVar "#a")    []                                                             (scons "Int")
    , makeNativeFun "List.reverse"  (Just $ listOf $ TVar "#a")    []                                                             (listOf $ TVar "#a")
    , makeNativeFun "List.take"     (Just $ listOf $ TVar "#a")    [scons "Int"]                                                  (listOf $ TVar "#a")
    , makeNativeFun "List.drop"     (Just $ listOf $ TVar "#a")    [scons "Int"]                                                  (listOf $ TVar "#a")
    , makeNativeFun "List.sort"     (Just $ listOf $ scons "Int")  []                                                             (listOf $ scons "Int")

    , makeNativeFun "List.fold"     (Just $ listOf $ TVar "#a")    [TVar "#b", TLam [TVar "#b", TVar "#a"] (TVar "#b")]           (TVar "#b")
    , makeNativeFun "List.map"      (Just $ listOf $ TVar "#a")    [TLam [TVar "#a"] (TVar "#b")]                                 (listOf $ TVar "#b")
    , makeNativeFun "List.zip"      (Just $ listOf $ TVar "#a")    [TLam [TVar "#a", TVar "#b"] (TVar "#c"), listOf $ TVar "#b"]  (listOf $ TVar "#c")
    , makeNativeFun "List.filter"   (Just $ listOf $ TVar "#a")    [TLam [TVar "#a"] (scons "Bool")]                              (listOf $ TVar "#a")

------------------
-- === Int === --
------------------

    , makeNativeFun "Int.=="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int./="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.<"         (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.<="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.>"         (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.>="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.min"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.max"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )

    , makeNativeFun "Int.+"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.*"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.-"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int./"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.%"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.^"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )

    , makeNativeFun "Int.negate"    (Just $ scons "Int")    []              (scons "Int" )
    , makeNativeFun "Int.abs"       (Just $ scons "Int")    []              (scons "Int" )
    , makeNativeFun "Int.signum"    (Just $ scons "Int")    []              (scons "Int" )

    , makeNativeFun "Int.pred"      (Just $ scons "Int")    []              (scons "Int" )
    , makeNativeFun "Int.succ"      (Just $ scons "Int")    []              (scons "Int" )
    , makeNativeFun "Int.even"      (Just $ scons "Int")    []              (scons "Bool")
    , makeNativeFun "Int.odd"       (Just $ scons "Int")    []              (scons "Bool")

    , makeNativeFun "Int.gcd"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.lcm"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )

    , makeNativeFun "Int.times"     (Just $ scons "Int")    [TVar "#a"]     (listOf $ TVar "#a")
    , makeNativeFun "Int.upto"      (Just $ scons "Int")    [scons "Int"]   (listOf $ scons "Int")

    , makeNativeFun "Int.toDouble"  (Just $ scons "Int")    []              (scons "Double")
    , makeNativeFun "Int.toString"  (Just $ scons "Int")    []              (scons "String")

--------------------
-- === Double === --
--------------------

    , makeNativeFun "Double.=="       (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double./="       (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.<"        (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.<="       (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.>"        (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.>="       (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.min"      (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.max"      (Just $ scons "Double")    [scons "Double"]   (scons "Double")

    , makeNativeFun "Double.+"        (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.*"        (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.-"        (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double./"        (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.**"       (Just $ scons "Double")    [scons "Double"]   (scons "Double")

    , makeNativeFun "Double.negate"   (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.abs"      (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.signum"   (Just $ scons "Double")    []                 (scons "Double")

    , makeNativeFun "Double.round"    (Just $ scons "Double")    []                 (scons "Int")
    , makeNativeFun "Double.ceiling"  (Just $ scons "Double")    []                 (scons "Int")
    , makeNativeFun "Double.floor"    (Just $ scons "Double")    []                 (scons "Int")

    , makeNativeFun "Double.exp"      (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.log"      (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.sqrt"     (Just $ scons "Double")    []                 (scons "Double")

    , makeNativeFun "Double.sin"      (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.cos"      (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.tan"      (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.asin"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.acos"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.atan"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.sinh"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.cosh"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.tanh"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.asinh"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.acosh"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.atanh"    (Just $ scons "Double")    []                 (scons "Double")

    , makeNativeFun "Double.toString" (Just $ scons "Double")    []                 (scons "String")
    , makeNativeFun "Double.toStringFormat" (Just $ scons "Double")    [scons "Int", scons "Int"]   (scons "String")

------------------
-- === Bool === --
------------------

    , makeNativeFun "Bool.=="       (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool./="       (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.<"        (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.<="       (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.>"        (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.>="       (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.min"      (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.max"      (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")

    , makeNativeFun "Bool.&&"       (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.||"       (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.not"      (Just $ scons "Bool")   []              (scons "Bool")

    , makeNativeFun "Bool.toString" (Just $ scons "Bool")   []              (scons "String")

--------------------
-- === String === --
--------------------

    , makeNativeFun "String.=="       (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String./="       (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.<"        (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.<="       (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.>"        (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.>="       (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.min"      (Just $ scons "String") [scons "String"]          (scons "String")
    , makeNativeFun "String.max"      (Just $ scons "String") [scons "String"]          (scons "String")

    , makeNativeFun "String.+"        (Just $ scons "String") [scons "String"]          (scons "String")
    , makeNativeFun "String.length"   (Just $ scons "String") []                        (scons "Int")
    , makeNativeFun "String.reverse"  (Just $ scons "String") []                        (scons "String")
    , makeNativeFun "String.take"     (Just $ scons "String") [scons "Int"]             (scons "String")
    , makeNativeFun "String.drop"     (Just $ scons "String") [scons "Int"]             (scons "String")
    , makeNativeFun "String.words"    (Just $ scons "String") []                        (listOf $ scons "String")
    , makeNativeFun "String.lines"    (Just $ scons "String") []                        (listOf $ scons "String")
    , makeNativeFun "String.join"     (Just $ scons "String") [listOf $ scons "String"] (scons "String")

    , makeNativeFun "String.toString" (Just $ scons "String") []                        (scons "String")

-----------------
-- === Ref === --
-----------------
    , makeNativeFun "ref"             Nothing                    [TVar "#a"]                    (refOf $ TVar "#a")
    , makeNativeFun "Ref.modify"      (Just $ refOf $ TVar "#a") [TLam [TVar "#a"] $ TVar "#a"] (refOf $ TVar "#a")
    , makeNativeFun "Ref.read"        (Just $ refOf $ TVar "#a") []                             (TVar "#a")

--------------------
--- === Misc === ---
--------------------

    , ("id",         makeId)
    , ("const",      makeConst)

    , makeNativeFun "app"          Nothing [TLam [TVar "#a"] (TVar "#b"),            (TVar "#a")]         (TVar "#b")
    , makeNativeFun "comp"         Nothing [TLam [TVar "#b"] (TVar "#c"), TLam [TVar "#a"] (TVar "#b")]   (TLam [TVar "#a"] (TVar "#c"))
    , makeNativeFun "flip"         Nothing [TLam [TVar "#a", TVar "#b"] (TVar "#c")]                      (TLam [TVar "#b", TVar "#a"] (TVar "#c"))

    , makeNativeFun "empty"        Nothing []                                                             (listOf $ TVar "#a")
    , makeNativeFun "singleton"    Nothing [TVar "#a"]                                                    (listOf $ TVar "#a")

    , makeNativeFun "switch"       Nothing [scons "Bool", TVar "#a", TVar "#a"]                           (TVar "#a")

    , makeNativeFun "readFile"     Nothing [scons "String"]                                               (scons "String")

    , makeNativeFun "mean"         Nothing [listOf $ scons "Double"]                                      (scons "Double")
    , makeNativeFun "differences"  Nothing [listOf $ scons "Int"]                                         (listOf $ scons "Int")
    , makeNativeFun "histogram"    Nothing [listOf $ scons "Int"]                                         (scons "Histogram")
    , makeNativeFun "primes"       Nothing [scons "Int"]                                                  (listOf $ scons "Int")
    , makeNativeFun "pi"           Nothing []                                                             (scons "Double")

----------------------
--- === Shapes === ---
----------------------

    , makeNativeFun "initTrans"                Nothing                         []                                       (scons "Transformation")
    , makeNativeFun "Transformation.scale"     (Just $ scons "Transformation") [scons "Double", scons "Double"]         (scons "Transformation")
    , makeNativeFun "Transformation.translate" (Just $ scons "Transformation") [scons "Double", scons "Double"]         (scons "Transformation")
    , makeNativeFun "Transformation.rotate"    (Just $ scons "Transformation") [scons "Double"]                         (scons "Transformation")
    , makeNativeFun "Transformation.reflect"   (Just $ scons "Transformation") []                                       (scons "Transformation")

    , makeNativeFun "square"                   Nothing                         [scons "Double"]                         (scons "Figure")
    , makeNativeFun "rectangle"                Nothing                         [scons "Double", scons "Double"]         (scons "Figure")
    , makeNativeFun "circle"                   Nothing                         [scons "Double"]                         (scons "Figure")

    , makeNativeFun "point"                    Nothing [scons "Double", scons "Double"]                                 (scons "Point")
    , makeNativeFun "initAttributes"           Nothing []                                                               (scons "Attributes")
    , makeNativeFun "color"                    Nothing [scons "Double", scons "Double", scons "Double", scons "Double"] (scons "Material")

    , makeNativeFun "Figure.primitive"         (Just $ scons "Figure")       [scons "Point", scons "Attributes"]        (scons "Primitive")

    , makeNativeFun "Primitive.shape"          (Just $ scons "Primitive")    []                                         (scons "Shape")
    , makeNativeFun "Shape.merge"              (Just $ scons "Shape")        [scons "Shape"]                            (scons "Shape")
    , makeNativeFun "Shape.subtract"           (Just $ scons "Shape")        [scons "Shape"]                            (scons "Shape")
    , makeNativeFun "Shape.intersect"          (Just $ scons "Shape")        [scons "Shape"]                            (scons "Shape")

    , makeNativeFun "Shape.surface"            (Just $ scons "Shape")        []                                         (scons "Surface")

    , makeNativeFun "geoElem"                  Nothing                       [listOf $ scons "Surface"]                 (scons "GeoComponent")
    , makeNativeFun "geoGroup"                 Nothing                       [listOf $ scons "Geometry"]                (scons "GeoComponent")
    , makeNativeFun "GeoComponent.geometry"    (Just $ scons "GeoComponent") [scons "Transformation", scons "Material"] (scons "Geometry")

    , makeNativeFun "Geometry.layer"           (Just $ scons "Geometry")     [listOf $ scons "Transformation"]          (scons "Layer")
    , makeNativeFun "graphics"                 Nothing                       [listOf $ scons "Layer"]                   (scons "Graphics")
    , makeNativeFun "Graphics.layers"          (Just $ scons "Graphics")     []                                         (listOf $ scons "Layer")

---------------------------
--- === Drawing API === ---
---------------------------

    , makeNativeFun "sampleData"     Nothing [TLam [scons "Double"] (scons "Double"),
                                              scons "Double", scons "Double", scons "Int"]                     (listOf $ scons "Point")

    , makeNativeFun "circleGeometry"         Nothing [scons "Double",                 scons "Material"]                (scons "Geometry")
    , makeNativeFun "squareGeometry"         Nothing [scons "Double",                 scons "Material"]                (scons "Geometry")
    , makeNativeFun "rectangleGeometry"      Nothing [scons "Double", scons "Double", scons "Material"]                (scons "Geometry")

    , makeNativeFun "axes"  Nothing [scons "Material", scons "Double", scons "Double", scons "Double", scons "Double", scons "Double"]  (listOf $ scons "Layer")
    , makeNativeFun "grid"  Nothing [scons "Material", scons "Double", scons "Double", scons "Double", scons "Double", scons "Double"]  (listOf $ scons "Layer")

    , makeNativeFun "scatterChart"    Nothing [scons "Material", scons "Figure", scons "Double",
                                                  scons "Double", scons "Double", scons "Double", scons "Double",
                                                  listOf $ scons "Point"]                                 (scons "Layer")
    , makeNativeFun "barChart"        Nothing [scons "Material", scons "Double",
                                                  scons "Double", scons "Double", scons "Double", scons "Double",
                                                  listOf $ scons "Point"]                                 (scons "Layer")
    , makeNativeFun "barChartLayers"  Nothing [scons "Material", scons "Double",
                                                  scons "Double", scons "Double", scons "Double", scons "Double",
                                                  listOf $ scons "Point"]                                 (scons "Graphics")

    , makeNativeFun "autoScatterChartInt"    Nothing [scons "Material", scons "Material", scons "Figure", scons "Double", listOf $ scons "Int"]              (scons "Graphics")
    , makeNativeFun "autoScatterChartDouble" Nothing [scons "Material", scons "Material", scons "Figure", scons "Double", listOf $ scons "Double"]           (scons "Graphics")

------------------------
--- === IoT Demo === ---
------------------------

    , makeNativeFun "temperature"     Nothing []                                  (scons "Temperature")
    , makeNativeFun "fan"             Nothing []                                  (scons "Fan")
    , makeNativeFun "controlPanel"    Nothing []                                  (scons "ControlPanel")

    , makeNativeFun "Temperature.inside"                (Just $ scons "Temperature")  [scons "Double"]                  (scons "Double")
    , makeNativeFun "Temperature.outside"               (Just $ scons "Temperature")  [scons "Double"]                  (scons "Double")
    , makeNativeFun "ControlPanel.temperatureThreshold" (Just $ scons "ControlPanel") [scons "Double"]                  (scons "Double")
    , makeNativeFun "ControlPanel.display"              (Just $ scons "ControlPanel") [scons "String", scons "String"]  (scons "String")
    , makeNativeFun "Fan.power"                         (Just $ scons "Fan")          [scons "Bool" ]                   (scons "String")

--------------------------
-- === Experimental === --
--------------------------

    , makeNativeFun "fix"          Nothing [TLam [TVar "#a"] (TVar "#a")]                                                              (TVar "#a")
    , makeNativeFun "app1to2"      Nothing [TLam [TVar "#a", TVar "#b"] (TVar "#c"), (TVar "#a")]                                      (TLam [TVar "#b"] (TVar "#c"))
    , makeNativeFun "app2to2"      Nothing [TLam [TVar "#a", TVar "#b"] (TVar "#c"), (TVar "#a"), (TVar "#b")]                         (TVar "#c")
    , makeNativeFun "app1to3"      Nothing [TLam [TVar "#a", TVar "#b", TVar "#c"] (TVar "#d"), (TVar "#a")]                           (TLam [TVar "#b", TVar "#c"] (TVar "#d"))
    , makeNativeFun "app2to3"      Nothing [TLam [TVar "#a", TVar "#b", TVar "#c"] (TVar "#d"), (TVar "#a"), (TVar "#b")]              (TLam [TVar "#c"] (TVar "#d"))
    , makeNativeFun "app3to3"      Nothing [TLam [TVar "#a", TVar "#b", TVar "#c"] (TVar "#d"), (TVar "#a"), (TVar "#b"), (TVar "#c")] (TVar "#d")
    , makeNativeFun "cycle3"       Nothing [TLam [TVar "#a", TVar "#b", TVar "#c"] (TVar "#d")]                                        (TLam [TVar "#b", TVar "#c", TVar "#a"] (TVar "#d"))
    , makeNativeFun "comp2"        Nothing [TLam [TVar "#b"] (TVar "#c"), TLam [TVar "#a", TVar "#a1"] (TVar "#b")]   (TLam [TVar "#a", TVar "#a1"] (TVar "#c"))
    , makeNativeFun "comp2to2"     Nothing [TLam [TVar "#t3", TVar "#t4"] (TVar "#t1"), TLam [TVar "#t3", TVar "#t4"] (TVar "#t2"), TLam [TVar "#t1", TVar "#t2"] (TVar "#t")]   (TLam [TVar "#t3", TVar "#t4"] (TVar "#t"))
    , makeNativeFun "comp3to2"     Nothing [TLam [TVar "#t4", TVar "#t5"] (TVar "#t1"), TLam [TVar "#t4", TVar "#t5"] (TVar "#t2"), TLam [TVar "#t4", TVar "#t5"] (TVar "#t3"), TLam [TVar "#t1", TVar "#t2", TVar "#t3"] (TVar "#t")]   (TLam [TVar "#t4", TVar "#t5"] (TVar "#t"))
    , ("p12",        makeConst)
    , ("p22",        makeFlipConst)
    , ("flipConst",  makeFlipConst)
    , ("flipConst2", makeFlipConst2)
    , makeNativeFun "retFun2"       Nothing [TLam [TVar "#a", TVar "#b"] (TVar "#c")]                                      (TLam [TVar "#a"] (TLam [TVar "#b"] (TVar "#c")))

    , makeNativeFun "testControls"  Nothing [scons "String", scons "Int", scons "Double", scons "Bool"]  (scons "String")

    ]

experimental = [ "fix"
               , "app1to2"
               , "app2to2"
               , "app1to3"
               , "app2to3"
               , "app3to3"
               , "cycle3"
               , "comp2"
               , "comp2to2"
               , "comp3to2"
               , "p12"
               , "p22"
               , "flipConst"
               , "flipConst2"
               , "retFun2"
               , "testControls"
               ]

symbolsNames :: [String]
symbolsNames = fst <$> symbolsList
