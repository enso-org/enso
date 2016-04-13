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
           | TVar String
           | TLam [TPRep] TPRep
           deriving (Show, Eq, Ord)

listOf :: TPRep -> TPRep
listOf t = TCons "List" [t]

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

makeId :: FunBuilderCtx(m) => m (String, Signature nodeRef)
makeId = do
    tpV   <- var "#idTp1"
    n     <- blank `typed` tpV
    return ("id", Signature Nothing [arg n] n)

symbols :: SymbolMap (NetLayers :<: Draft Static) NetGraph
symbols = Map.fromList $ fmap (\(n, b) -> (QualPath.mk (n :: String), makeFunction b)) symbolsList

symbolsList =
------------------
-- === Int === --
------------------
    [ makeNativeFun "Int.+"         (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
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

    , makeNativeFun "Int.=="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int./="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.<"         (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.<="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.>"         (Just $ scons "Int")    [scons "Int"]   (scons "Bool")
    , makeNativeFun "Int.>="        (Just $ scons "Int")    [scons "Int"]   (scons "Bool")

    , makeNativeFun "Int.min"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.max"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.gcd"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )
    , makeNativeFun "Int.lcm"       (Just $ scons "Int")    [scons "Int"]   (scons "Int" )

    , makeNativeFun "Int.toString"  (Just $ scons "Int")    []              (scons "String")
    , makeNativeFun "Int.toDouble"  (Just $ scons "Int")    []              (scons "Double")
    , makeNativeFun "Int.times"     (Just $ scons "Int")    [TVar "#times"] (listOf $ TVar "#times")
    , makeNativeFun "Int.upto"      (Just $ scons "Int")    [scons "Int"]   (listOf $ scons "Int")

    {-, makeNativeFun "List.head"  (Just $ listOf $ TVar "#head")        []                                                                    (TVar "#head")-}
    {-, makeNativeFun "List.map"   (Just $ listOf $ TVar "#map")         [TLam [TVar "#map"] (TVar "#map1")]                                   (listOf $ TVar "#map1")-}
    {-, makeNativeFun "add"       Nothing                               [scons "Int", scons "Int"]                                            (scons "Int" )-}
    {-, ("List.fold"  (Just $ listOf $ TVar "#foldB")       [TVar "#foldA", TLam [TVar "#foldA", TVar "#foldB"] (TVar "#foldA")]  (TVar "#foldA"))-}
    {-, ("succ"       Nothing                               [scons "Int"]                                                         (scons "Int" ))-}
    {-, ( "(+)"       Nothing                               [scons "Int", scons "Int"]                                            (scons "Int" ))-}
    {-, ( "(*)"       Nothing                               [scons "Int", scons "Int"]                                            (scons "Int" ))-}
    -- forM  :: Monad m => [a] -> (a -> m b) -> m [b]
    -- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
    ]
--------------------
-- === Double === --
--------------------
    {-, ("Double.=="        , makePureFun "(==)"                    (Just $ scons "Double")    [scons "Double"]   (scons "Bool"))-}
    {-, ("Double./="        , makePureFun "(/=)"                    (Just $ scons "Double")    [scons "Double"]   (scons "Bool"))-}
    {-, ("Double.<"         , makePureFun "(<)"                     (Just $ scons "Double")    [scons "Double"]   (scons "Bool"))-}
    {-, ("Double.<="        , makePureFun "(<=)"                    (Just $ scons "Double")    [scons "Double"]   (scons "Bool"))-}
    {-, ("Double.>"         , makePureFun "(>)"                     (Just $ scons "Double")    [scons "Double"]   (scons "Bool"))-}
    {-, ("Double.>="        , makePureFun "(>=)"                    (Just $ scons "Double")    [scons "Double"]   (scons "Bool"))-}
    {-, ("Double.min"       , makePureFun "min"                     (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}
    {-, ("Double.max"       , makePureFun "max"                     (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}

    {-, ("Double.+"         , makePureFun "(+)"                     (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}
    {-, ("Double.*"         , makePureFun "(*)"                     (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}
    {-, ("Double.-"         , makePureFun "(-)"                     (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}
    {-, ("Double./"         , makePureFun "(/)"                     (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}
    {-, ("Double.**"        , makePureFun "(**)"                    (Just $ scons "Double")    [scons "Double"]   (scons "Double"))-}
    {-, ("Double.negate"    , makePureFun "negate"                  (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.abs"       , makePureFun "abs"                     (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.signum"    , makePureFun "signum"                  (Just $ scons "Double")    []                 (scons "Double"))-}

    {-, ("Double.round"     , makePureFun "round"                   (Just $ scons "Double")    []                 (scons "Int"))-}
    {-, ("Double.ceiling"   , makePureFun "ceiling"                 (Just $ scons "Double")    []                 (scons "Int"))-}
    {-, ("Double.floor"     , makePureFun "floor"                   (Just $ scons "Double")    []                 (scons "Int"))-}

    {-, ("Double.exp"     , makePureFun "exp"                       (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.log"     , makePureFun "log"                       (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.sqrt"    , makePureFun "sqrt"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.sin"     , makePureFun "sin"                       (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.cos"     , makePureFun "cos"                       (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.tan"     , makePureFun "tan"                       (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.asin"    , makePureFun "asin"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.acos"    , makePureFun "acos"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.atan"    , makePureFun "atan"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.sinh"    , makePureFun "sinh"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.cosh"    , makePureFun "cosh"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.tanh"    , makePureFun "tanh"                      (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.asinh"   , makePureFun "asinh"                     (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.acosh"   , makePureFun "acosh"                     (Just $ scons "Double")    []                 (scons "Double"))-}
    {-, ("Double.atanh"   , makePureFun "atanh"                     (Just $ scons "Double")    []                 (scons "Double"))-}

{--------------------}
{--- === Bool === ---}
{--------------------}

    {-, ("Bool.&&"       , makePureFun "(&&)"                    (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool.||"       , makePureFun "(||)"                    (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool.not"      , makePureFun "not"                     (Just $ scons "Bool")   []              (scons "Bool"))-}
    {-, ("Bool.=="       , makePureFun "(==)"                    (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool./="       , makePureFun "(/=)"                    (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool.<"        , makePureFun "(<)"                     (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool.<="       , makePureFun "(<=)"                    (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool.>"        , makePureFun "(>)"                     (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}
    {-, ("Bool.>="       , makePureFun "(>=)"                    (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool"))-}

{----------------------}
{--- === String === ---}
{----------------------}

    {-, ("String.length"  , makePureFun "length"                 (Just $ scons "String") []                        (scons "Int"))-}
    {-, ("String.reverse" , makePureFun "reverse"                (Just $ scons "String") []                        (scons "String"))-}
    {-, ("String.words"   , makePureFun "words"                  (Just $ scons "String") []                        (listOf $ scons "String"))-}
    {-, ("String.lines"   , makePureFun "lines"                  (Just $ scons "String") []                        (listOf $ scons "String"))-}
    {-, ("String.join"    , makePureFun "intercalate"            (Just $ scons "String") [listOf $ scons "String"] (scons "String"))-}
    {-, ("String.+"       , makePureFun "(++)"                   (Just $ scons "String") [scons "String"]          (scons "String"))-}
    {-, ("String.=="      , makePureFun "(==)"                   (Just $ scons "String") [scons "String"]          (scons "Bool"))-}
    {-, ("String./="      , makePureFun "(/=)"                   (Just $ scons "String") [scons "String"]          (scons "Bool"))-}
    {-, ("String.<"       , makePureFun "(<)"                    (Just $ scons "String") [scons "String"]          (scons "Bool"))-}
    {-, ("String.<="      , makePureFun "(<=)"                   (Just $ scons "String") [scons "String"]          (scons "Bool"))-}
    {-, ("String.>"       , makePureFun "(>)"                    (Just $ scons "String") [scons "String"]          (scons "Bool"))-}
    {-, ("String.>="      , makePureFun "(>=)"                   (Just $ scons "String") [scons "String"]          (scons "Bool"))-}

{--------------------}
{--- === List === ---}
{--------------------}

    {-, ("List.length"      , makePureFun "length"                 (Just $ listOf $ TVar "#len")         []                     (scons "Int"))-}
    {-, ("List.reverse"     , makePureFun "reverse"                (Just $ listOf $ TVar "#reverse")     []                     (listOf $ TVar "#reverse"))-}
    {-, ("List.+"           , makePureFun "(++)"                   (Just $ listOf $ TVar "#lpl")         [listOf $ TVar "#lpl"] (listOf $ TVar "#lpl"))-}
    {-, ("List.mapLength"   , makePureFun "(map length)"           (Just $ listOf $ listOf $ TVar "#ml") []                     (listOf $ scons "Int"))-}
    {-, ("List.mapToDouble" , makePureFun "(map fromIntegral)"     (Just $ listOf $ scons "Int")         []                     (listOf $ scons "Double"))-}
    {-, ("List.mapLog"      , makePureFun "(map log)"              (Just $ listOf $ scons "Double")      []                     (listOf $ scons "Double"))-}
    {-, ("List.sort"        , makePureFun "sort"                   (Just $ listOf $ TVar "#sort")        []                     (listOf $ TVar "#sort"))-}
    {-, ("empty"            , makePureFun "([])"                   Nothing                               []                     (listOf $ TVar "#empty"))-}

{--------------------}
{--- === Misc === ---}
{--------------------}

    {-, ("readFile"       , makeNativeFun  "readFile"                                      Nothing [scons "String"]                        (scons "String"))-}
    {-, ("id"             , makeId)-}
    {-, ("switch"         , makePureFun "(\\x y z -> if x then y else z)"                  Nothing [scons "Bool", TVar "#if", TVar "#if"]  (TVar "#if"))-}
    {-, ("histogram"      , makePureFun "(map (\\l -> (head l, length l)) . group . sort)" Nothing [listOf $ scons "Int"]                  (scons "Histogram"))-}
    {-, ("primes"         , makePureFun primesBody                                         Nothing [scons "Int"]                           (listOf $ scons "Int"))-}
    {-, ("differences"    , makePureFun differencesBody                                    Nothing [listOf $ scons "Int"]                  (listOf $ scons "Int"))-}
    {-, ("mean"           , makePureFun meanBody                                           Nothing [listOf $ scons "Double"]               (scons "Double"))-}

{--------------------}
{--- === Tests === ---}
{--------------------}


{-primesBody :: String-}
{-primesBody = intercalate " ; " $-}
    {-[ "(\\x -> let primes' = 2 : filter isPrime [3, 5..]"-}
    {-, "          isPrime n = not $ any (\\p -> n `rem` p == 0) (takeWhile (\\p -> p*p <= n) primes')"-}
    {-, "      in take x $ primes')"-}
    {-]-}

{-differencesBody :: String-}
{-differencesBody = "(\\l -> zipWith (flip subtract) (if (null l) then [] else tail l) l)"-}

{-meanBody :: String-}
{-meanBody = "(uncurry (/) . foldr (\\e (s, c) -> (e + s, c + 1)) (0, 0))"-}
symbolsNames :: [String]
symbolsNames = fst <$> symbolsList
