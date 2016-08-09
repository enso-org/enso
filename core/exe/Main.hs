{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# BOOSTER  VariantCase               #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}

module Main where

import Data.Graph          hiding (Dynamic)
import Data.Graph.Builders
import Prologue            hiding (Symbol, Cons, Num, Version, cons, read, ( # ), Enum)

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder       hiding (get)
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
import           Data.Prop
import           Data.Record              hiding (Cons, Layout, cons, Value)
import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import qualified Data.Graph.Builder.Class                        as Graph
import           Data.Graph.Builder.Ref                          as Ref
import           Luna.Compilation.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Compilation.Pass.Inference.Importing       (SymbolImportingPass (..))
import qualified Luna.Compilation.Pass.Inference.Importing       as Importing
import           Luna.Compilation.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Compilation.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Compilation.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Compilation.Pass.Utils.Literals            as LiteralsUtils
import           Luna.Compilation.Stage.TypeCheck                (Loop (..), Sequence (..))
import qualified Luna.Compilation.Stage.TypeCheck                as TypeCheck
import qualified Luna.Compilation.Stage.TypeCheck.Class          as TypeCheckState
import qualified Luna.Config.Env                                 as Env
import qualified Luna.Library.Symbol                             as Symbol
import           Luna.Pretty.GraphViz
import           Luna.Runtime.Dynamics                           (Dynamics, Dynamic, Static)
import qualified Luna.Runtime.Dynamics                           as Runtime
import           Luna.Syntax.Model.Layer                         ((:<), (:<:))
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node          hiding (star2, blank, unify)
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetGraph2, NetLayers, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Luna.Syntax.Model.Network.Class                 (Network)
import qualified Luna.Syntax.Model.Network.Term                  as Net
import           Luna.Syntax.Term                               (OverBuilder, Layout_OLD, ExprRecord, overbuild, AnyExpr)
import qualified Old.Luna.Syntax.Term.Class                           as Term
import           Luna.Syntax.Term.Expr.Format                         (SuperFormats, Draft(Draft), Literal(Literal), Value(Value))
import qualified Old.Luna.Syntax.Term.Expr.Lit                            as Lit

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty) -- refactor empty to another library

import           Luna.Syntax.Model.Network.Builder.Class ()
import qualified Luna.Syntax.Model.Network.Builder.Class as XP
import           Luna.Syntax.Model.Network.Builder.Term.Class (star2, ExprBuilder)

import qualified Data.Record                  as Record
import qualified Data.Graph.Builder                      as GraphBuilder

import Data.Shell as Shell
import Data.Shell (Stack(..))
import Data.Cover
import Type.Applicative
import Luna.Syntax.Term.Expr hiding (Data)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.Syntax.Term.Expr.Class as TEST
import Luna.Syntax.Term.Expr.Class (All, cons2, Layout(..), Term, Term', Data)
import Data.Record.Model.Masked (encodeStore, encodeData2, Store2, Slot(Slot), Enum, Raw, Mask)

import Luna.Syntax.Model.Network.Builder.Term.Class (TermBuilder)
import Prelude (error, undefined)
import Type.List (In)
import Data.Container.Hetero (Elems)
import GHC.TypeLits hiding (Symbol)
import GHC.TypeLits (ErrorMessage(Text))
import Luna.Syntax.Term.Expr.Atom (Atoms)

import qualified Luna.Syntax.Term.Expr.Symbol as Symbol
import Control.Lens.Property
import Luna.Syntax.Term.Expr.Format (Format)
import TH
import qualified Data.Vector as V
import GHC.Prim (Any)

import Unsafe.Coerce (unsafeCoerce)

title s = putStrLn $ "\n" <> "-- " <> s <> " --"



--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast (Edge e) = cast e
--instance Castable (Arc src tgt) (Arc src' tgt') => Castable (Arc src tgt) (Arc src' tgt') where cast e = Edge $ cast e
-- --------------------------------------
--  !!! KEEP THIS ON THE BEGINNING !!! --
-- --------------------------------------
-- - vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv ---


prebuild :: IO (Ref Node (NetLayers :<: Net.Draft Static), NetGraph)
prebuild = runBuild2 def $ do
    star
    star
    star

prebuild2 :: IO (Ref Node (NetLayers :<: Net.Draft Static), NetGraph)
prebuild2 = runBuild3 def $ do
    var2 "ala"

tst :: (TermBuilder Star m a, Inferable ELEMENT a m, ExprBuilder Draft m a) => m a
tst = do
    star2

-- tst' :: (Inferable ELEMENT a m, ExprBuilder Draft m a) => m (Ref Node (ls :< Expr t Draft Static))
-- tst' = do
--     star2

-- Ref' Node (ls :< Expr t Draft Static)
--
-- --------------
-- Ref (Term t Draft Static Any)
-- Ref (Link (Term t Draft Static Any))
--
-- read :: Ref a -> m a
-- write :: a -> m (Ref a)


-- var :: Name -> KnownTerm t fmt dyn Var
-- unify :: Ref (Term t fmt dyn a) -> Ref (Term t fmt dyn b) -> Ref (KnownTerm t fmt dyn Unify)

-- contains layers
{-
- nie trzeba uncoverowac i mozna sie bezposrednio ladnie na typach pattern-matchowac
- bardziej jednolita architektura - nie wymusza layerowania
- konstruktory moga byc dokladniejszych typow niz `m a`
-}




-- Term (Network ls) Draft  rt

tst2 :: (TermBuilder Star m (Ptr t tgt), PointedM t tgt g m a, MonadBuilder g m, Inferable ELEMENT (Ptr t tgt) m, ExprBuilder Draft m (Ptr t tgt)) => m a
tst2 = do
    s <- tst
    read s

-- read :: a -> m (Expr a)
--
-- read :: Mark Draft Static a -> m (ls :< Expr Draft Static a)
--
-- Ref Node (ls :<: Net.Draft Static) --> read
--          (ls :<: Net.Draft Static)
--
--
-- mkterm :: m a
-- -- a moze byc np. Pointerem
--
-- Mark Draft Static (Ref Node (NetLayers :<: Net.Draft Static)) --> read
--                             (NetLayers :<: Net.Draft Static)
--
--
-- Mark Draft Static (NetLayers :< Loc Node) --> read




runBuild (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Net.Draft Static)))
                             $ runNetworkBuilderT g m


runBuild2 (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Net.Draft Static)))
                             $ runNetworkBuilderT2 g m


runBuild3 (g :: NetGraph) m = runInferenceT ELEMENT (Proxy :: Proxy (Ref Node (NetLayers :<: Net.Draft Static)))
                             . runNetworkBuilderT2 g
                             . XP.runNetworkBuilderT
                             $ m

evalBuild = fmap snd ∘∘ runBuild
--TODO: test1
-- TODO: test2




------------------
-- === Refs === --
------------------

type family Referred a
newtype Ref2 a = Ref2 (Referred a)
makeWrapped ''Ref2

class Monad m => Referable m a where
    refer'  :: a -> m (Referred a)
    read2'  :: Referred a -> m a
    write2' :: Referred a -> a -> m ()

refer :: Referable m a => a -> m (Ref2 a)
refer = Ref2 <∘> refer' ; {-# INLINE refer #-}

read2 :: Referable m a => Ref2 a -> m a
read2 = read2' ∘ unwrap' ; {-# INLINE read2 #-}

write2 :: Referable m a => Ref2 a -> a -> m ()
write2 = write2' ∘ unwrap' ; {-# INLINE write2 #-}

-- === Instances === --

deriving instance Show (Referred a) => Show (Ref2 a)


------------------
-- === Binds === --
------------------

type family Binded a
newtype Bind a = Bind (Binded a)
makeWrapped ''Bind

class Monad m => Bindable m a where
    bind' :: a -> m (Binded a)

bind :: Bindable m a => a -> m (Bind a)
bind = Bind <∘> bind' ; {-# INLINE bind #-}



------------------------------------
-- === Example implementation === --
------------------------------------


----------------------
-- === IntLayer === --
----------------------

-- === Definitions === --

data IntLayer = IntLayer deriving (Show)

type instance LayerData (NetLayer t IntLayer) = Int

-- === Instances === --

instance Monad m => Creator m (Layer (NetLayer t IntLayer)) where create = return $ Layer 7


---------------------
-- === Network === --
---------------------

-- == Definitions === --

data Net (ls :: [*])
newtype NetLayer t a = NetLayer a deriving (Show, Functor, Traversable, Foldable)
newtype NetRef     a = NetRef   a deriving (Show, Functor, Traversable, Foldable)


-- === Instances === --

-- Type relations
-- type instance LayerData (NetLayer t a) = LayerData a

-- Layout_OLD
type TermShell ls term rec = (NetLayer term <$> ls) :| rec
-- type instance Layout_OLD (Net ls) fmt dyn sel = TermShell ls (Expr (Net ls) fmt dyn sel) (ExprRecord (Net ls) fmt dyn sel Int) -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)
type instance Layout_OLD (Net ls) fmt dyn sel = ExprRecord (Net ls) fmt dyn sel Int -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)

-- Shell
type instance Shell.Access l (Expr (Net ls) fmt dyn sel) = NetLayer (Expr (Net ls) fmt dyn sel) l

-- Ref
type instance Referred (Expr (Net ls) fmt dyn sel) = NetRef (Expr (Net ls) fmt dyn sel)
instance Monad m => Referable m (Expr (Net ls) fmt dyn sel) where refer' = return ∘ NetRef



----------------------------------



type TRex2 t fmt dyn sel = ExprRecord t fmt dyn sel Int -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)


--
--
-- blank  = () ^. from symbolArgs :: Symbol.Data Blank Static Int
-- -- -- t1 = Record.cons blank :: AnyExpr SNet Draft Static
-- t1 = Record.cons blank :: TRex2 t Draft Static 'Nothing
--
-- t2 = runIdentity $ overbuild t1 :: AnyExpr (Net '[IntLayer]) Draft Static
--
-- -- t2' = runIdentity $ buildElem $ blank :: AnyExpr (Net '[IntLayer]) Draft Static
--
-- t3 = runIdentity $ refer t2

-- t4 :: OverBuilder Identity (Expr t fmt dyn sel) => a -> Ref2 (Expr t fmt dyn sel)
-- t4 a = runIdentity $ foox a
--
--
-- type Foox = Record.Variants (RecordOf (AnyExpr SNet Draft Static))


instance (Creator m c, OverBuilder m a) => OverBuilder m (Cover c a) where
    overbuild a = Cover <$> create <*> overbuild a

type ElemBuilder atom m a = (Record.Cons atom (RecordOf a), OverBuilder m a)
type RefBuilder  atom m a = (ElemBuilder atom m a, Referable m a)

buildElem :: ElemBuilder atom m a => atom -> m a
buildElem = overbuild ∘ Record.cons ; {-# INLINE buildElem #-}

buildRef :: RefBuilder atom m a => atom -> m (Ref2 a)
buildRef = buildElem >=> refer ; {-# INLINE buildRef #-}

-- powinnismy wprowadzic abstrakcje Bind, ktora moglaby miec source i target i w networku reprezentowalaby connection

--DataConI Luna.Syntax.Term.Expr.Symbol.Unify (ForallT [KindedTV dyn_1761715480 StarT,KindedTV a_1761715481 StarT] [] (AppT (AppT ArrowT (VarT a_1761715481)) (AppT (AppT ArrowT (VarT a_1761715481)) (AppT (AppT (AppT (ConT Luna.Syntax.Term.Expr.Symbol.Data) (ConT Luna.Syntax.Term.Expr.Atom.Unify)) (VarT dyn_1761715480)) (VarT a_1761715481))))) Luna.Syntax.Term.Expr.Symbol.Data

-- class Monad m => OverBuilder m a where
--     overbuild :: RecordOf a -> m a

-- tx = Record.cons Atom.Blank :: Term Int Draft Runtime.Dynamic

data Network2 = Network2 deriving (Show)

-- Term Network2 '[Int] Static Draft Static Draft
-- Expr3 Network2 '[] '[Int] Static Draft Static App
--
-- Expr3 Network2 '[] '[Int] (Layout Static Draft) (Layout Static App)


data ZZ = AA | BB


-- #define CASE $(testTH [| case
-- #define ESAC {--}|])

runCase :: Term binding attrs layers model scope -> [Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    s   = unwrap' $ get @Symbol $ unwrap' $ get @Data el
    idx = unwrap' $ get @Atom $ unwrap' $ get @Data el
{-# INLINE runCase #-}


matchx f = f . unsafeCoerce
{-# INLINE matchx #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}

--
type A1 = Term Network2 '[] '[Int] (Layout Dynamic Draft) (Layout Static Unify)
type A2 = Term Network2 '[] '[Int] (Layout Static Value) (Layout Dynamic Draft)

type Expr  dyn scope dyn' scope' = Term Network2 '[] '[Int] (Layout dyn scope) (Layout dyn' scope')
type Expr' dyn scope             = Expr dyn scope dyn scope

-- Ref Node (Expr Static Value)


instance Monad m => TEST.Cons2 a m (Ref Edge x) where
    cons2 _ = return $ Ptr 0

type instance TEST.Bound Network2 dict = Ref Edge (Expr' (Get Dynamics (Get TEST.Scope dict))
                                                         (Get Format   (Get TEST.Scope dict))
                                                  )
blank :: Symbol.Data Blank dyn a
blank = () ^. from symbolArgs :: Symbol.Data Blank dyn a

unify :: a -> a -> Symbol.Data Unify dyn a
unify l r = view (from symbolArgs) (l,r)

main :: IO ()
main = do
    print blank
    print $ (runIdentity (encodeStore blank) :: Store2 '[ 'Slot Enum Atom, 'Slot Mask Format, 'Slot Raw Symbol ])
    let e1 = (runIdentity (cons2 blank) :: Term Network2 '[] '[Int] (Layout Static Draft) (Layout Static Draft))
    let e2 = (runIdentity (cons2 blank) :: Expr' Static Draft)
    let eb1 = (runIdentity (cons2 blank) :: Ref Edge (Expr' Static Draft))
    let eu1 = (runIdentity (cons2 $ unify eb1 eb1) :: Expr' Static Draft)
    print e2

    putStrLn ""
    print $ get @Symbol $ unwrap' $ get @Data e1
    print $ unwrap' $ get @Atom $ unwrap' $ get @Data e1
    -- print $ unwrap' $ get @Atom $ unwrap' $ get @Symbol e1
    -- let a = AA
    -- case a of
    --     BB -> print "tsr"

    -- case3 (view (List.access' ExprData) e1) $
    --     of3 $ \(Symbol.Unify l r) -> (print "hello" :: IO ())
            --
            -- case3 (get @Symbol e1) $ do
            --     of3 $ \(Symbol.Unify l r) -> print "hello"
            --     of3 $ \(Symbol.Blank)     -> print "hello2"

    -- CASE e1 of
    --     Symbol.Unify l r -> print "hello"
    --     Symbol.Blank     -> print "hello2"
    -- ESAC
    --



    -- $(testTH2 'e1 ( \(Symbol.Unify l r) -> print "hello"  :: IO ()
    --               , \Symbol.Blank       -> print "hello2" :: IO ()
    --               )
    --  )


    let xx = (let blank = 2 in blank)


    let { exp = e1
    ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
        ala = 11
    ;f2 = matchx $ \Symbol.Blank       -> (print "hello2" )
       where ala = 11
    } in $(testTH2 'exp [ [p|Symbol.Unify l r|], [p|Symbol.Blank|] ] ['f1, 'f2])

    case' e1 of
        Symbol.Unify l r -> print 11
        Symbol.Blank     -> case' e1 of
            Symbol.Unify l r -> print "hello"
            Symbol.Blank     -> print "hello3x"


    -- $(testTH2 'exp [ [p|Symbol.Unify l r|] ] ['f1])


    --
    -- case' e1 of
    --     Symbol.Unify l r -> print "hello"
    --     Symbol.Blank     -> print "hello2"


    -- runCase e1 [
    --     matchx $ \(Symbol.Unify l r) -> print "hello"
    --     matchx $ \Symbol.Blank       -> print "hello2"
    --
    -- ]

    -- runCase e1 $ [ \_ -> print "1"
    --                                             , \_ -> print "2"
    --                                             ]

    return ()

    -- print $ view (List.access' ExprData) e1 -- Refactor, List is in Fact TMap



    -- case' e1 $
    --     of'                   $ \(Symbol.Unify l r) -> print "hello"
    --     match  Unify          $ \unify              -> print "hello2"
    --     match  (Unify || App) $ \ua                 -> print "hello3"
    --     static                $ \s                  -> print "static"
    --     :_                    $                        print "oh!"


    -- E.main

    -- TEST.main
    -- (_,  g :: NetGraph ) <- prebuild2
    -- -- renderAndOpen [ ("xg", "xg", g)
    -- --               ]
    -- main2
    -- main3
        --
        -- main2 = do
        --
        --     print t2
        --     -- print $ t2 ^. Shell.access' IntLayer
        --
        --     caseTest t2 $ do
        --         of' $ \Blank -> print "it is Blank!"
        --
        -- main3 :: IO ()
        -- main3 = do
        --     (n,g) <- flip GraphBuilder.runT (def :: NetGraph2) $ do
        --         (n1 :: Ref2 (AnyExpr (Net '[IntLayer]) Draft Static)) <- buildRef blank
        --         return n1
        --     print n
        --
        --     return ()


-- refactor to Data.Construction ?
