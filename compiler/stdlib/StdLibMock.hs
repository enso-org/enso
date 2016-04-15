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

makeId :: FunBuilderCtx(m) => m (Signature nodeRef)
makeId = do
    tpV   <- var "#idTp1"
    n     <- blank `typed` tpV
    return $ Signature Nothing [arg n] n

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

--------------------
-- === Double === --
--------------------
    , makeNativeFun "Double.=="      (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double./="      (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.<"       (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.<="      (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.>"       (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.>="      (Just $ scons "Double")    [scons "Double"]   (scons "Bool")
    , makeNativeFun "Double.min"     (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.max"     (Just $ scons "Double")    [scons "Double"]   (scons "Double")

    , makeNativeFun "Double.+"       (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.*"       (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.-"       (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double./"       (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.**"      (Just $ scons "Double")    [scons "Double"]   (scons "Double")
    , makeNativeFun "Double.negate"  (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.abs"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.signum"  (Just $ scons "Double")    []                 (scons "Double")

    , makeNativeFun "Double.round"   (Just $ scons "Double")    []                 (scons "Int")
    , makeNativeFun "Double.ceiling" (Just $ scons "Double")    []                 (scons "Int")
    , makeNativeFun "Double.floor"   (Just $ scons "Double")    []                 (scons "Int")

    , makeNativeFun "Double.exp"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.log"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.sqrt"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.sin"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.cos"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.tan"     (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.asin"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.acos"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.atan"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.sinh"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.cosh"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.tanh"    (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.asinh"   (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.acosh"   (Just $ scons "Double")    []                 (scons "Double")
    , makeNativeFun "Double.atanh"   (Just $ scons "Double")    []                 (scons "Double")

------------------
-- === Bool === --
------------------

    , makeNativeFun "Bool.&&"  (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.||"  (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.not" (Just $ scons "Bool")   []              (scons "Bool")
    , makeNativeFun "Bool.=="  (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool./="  (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.<"   (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.<="  (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.>"   (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")
    , makeNativeFun "Bool.>="  (Just $ scons "Bool")   [scons "Bool"]  (scons "Bool")

--------------------
-- === String === --
--------------------

    , makeNativeFun "String.length"  (Just $ scons "String") []                        (scons "Int")
    , makeNativeFun "String.reverse" (Just $ scons "String") []                        (scons "String")
    , makeNativeFun "String.words"   (Just $ scons "String") []                        (listOf $ scons "String")
    , makeNativeFun "String.lines"   (Just $ scons "String") []                        (listOf $ scons "String")
    , makeNativeFun "String.join"    (Just $ scons "String") [listOf $ scons "String"] (scons "String")
    , makeNativeFun "String.+"       (Just $ scons "String") [scons "String"]          (scons "String")
    , makeNativeFun "String.=="      (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String./="      (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.<"       (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.<="      (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.>"       (Just $ scons "String") [scons "String"]          (scons "Bool")
    , makeNativeFun "String.>="      (Just $ scons "String") [scons "String"]          (scons "Bool")

------------------
-- === List === --
------------------

    , makeNativeFun "List.length"  (Just $ listOf $ TVar "#len")         []                                                                    (scons "Int")
    , makeNativeFun "List.reverse" (Just $ listOf $ TVar "#reverse")     []                                                                    (listOf $ TVar "#reverse")
    , makeNativeFun "List.drop"    (Just $ listOf $ TVar "#a")           [scons "Int"]                                                         (listOf $ TVar "#a")
    , makeNativeFun "List.+"       (Just $ listOf $ TVar "#lpl")         [listOf $ TVar "#lpl"]                                                (listOf $ TVar "#lpl")
    , makeNativeFun "List.fold"    (Just $ listOf $ TVar "#foldB")       [TVar "#foldA", TLam [TVar "#foldA", TVar "#foldB"] (TVar "#foldA")]  (TVar "#foldA")
    , makeNativeFun "List.map"     (Just $ listOf $ TVar "#map")         [TLam [TVar "#map"] (TVar "#map1")]                                   (listOf $ TVar "#map1")
    , makeNativeFun "List.zip"     (Just $ listOf $ TVar "#a")           [TLam [TVar "#a", TVar "#b"] (TVar "#c"), listOf $ TVar "#b"]         (listOf $ TVar "#c")
    , makeNativeFun "List.sort"    (Just $ listOf $ scons "Int")         []                                                                    (listOf $ scons "Int")
    {-, ("empty"            , makePureFun "([])"                   Nothing                               []                     (listOf $ TVar "#empty"))-}


{--------------------}
{--- === Misc === ---}
{--------------------}

    , ("id",         makeId)
    , makeNativeFun "readFile"     Nothing [scons "String"]                        (scons "String")
    , makeNativeFun "switch"       Nothing [scons "Bool", TVar "#a", TVar "#a"]    (TVar "#a")
    , makeNativeFun "histogram"    Nothing [listOf $ scons "Int"]                  (scons "Histogram")
    , makeNativeFun "primes"       Nothing [scons "Int"]                           (listOf $ scons "Int")
    , makeNativeFun "differences"  Nothing [listOf $ scons "Int"]                  (listOf $ scons "Int")
    ]

{--------------------}
{--- === Tests === ---}
{--------------------}


{-primesBody :: String-}
{-primesBody = intercalate " ; " $-}
    {-[ "(\\x -> let primes' = 2 : filter isPrime [3, 5..]"-}
    {-, "          isPrime n = not $ any (\\p -> n `rem` p == 0) (takeWhile (\\p -> p*p <= n) primes')"-}
    {-, "      in take x $ primes')"-}
    {-]-}

symbolsNames :: [String]
symbolsNames = fst <$> symbolsList
