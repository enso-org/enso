{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE PolyKinds                 #-}

module Main where


import Data.Graph
import Data.Graph.Builders
import Prologue            hiding (Cons, Num, Version, cons, read, ( # ))

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
import           Data.Prop
import           Data.Record              hiding (Cons, Layout, cons)
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
import           Luna.Runtime.Dynamics                           (Dynamic, Static)
import qualified Luna.Runtime.Dynamics                           as Runtime
import           Luna.Syntax.Model.Layer                         ((:<), (:<:))
import           Luna.Syntax.Model.Network.Builder               (rebuildNetwork')
import           Luna.Syntax.Model.Network.Builder.Node          hiding (star2)
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetLayers, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Luna.Syntax.Model.Network.Class                 (Network)
import qualified Luna.Syntax.Model.Network.Term                  as Net
import           Luna.Syntax.Term.Class_OLD                          (OverBuilder, Layout2, Term2, TermRecord2, overbuild, AnyTerm, Term)
import qualified Luna.Syntax.Term.Class_OLD                           as Term
import           Luna.Syntax.Term.Format                         (Draft(Draft))
import qualified Luna.Syntax.Term.Lit                            as Lit

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E

import           Luna.Syntax.Model.Network.Builder.Class ()
import qualified Luna.Syntax.Model.Network.Builder.Class as XP
import           Luna.Syntax.Model.Network.Builder.Term.Class (star2, ExprBuilder)

import qualified Data.Record                  as Record

import Data.Shell as Shell
import Data.Cover
import Type.Applicative
import Luna.Syntax.Term.Expr


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

tst :: (Inferable ELEMENT a m, ExprBuilder Draft m a) => m a
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

tst2 :: (PointedM t tgt g m a, MonadBuilder g m, Inferable ELEMENT (Ptr t tgt) m, ExprBuilder Draft m (Ptr t tgt)) => m a
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



------------------------------------
-- === Example implementation === --
------------------------------------


----------------------
-- === IntLayer === --
----------------------

-- === Definitions === --

data IntLayer = IntLayer deriving (Show)

type instance LayerData IntLayer = Int

-- === Instances === --

instance Monad m => Creator m (Layer (NetLayer t IntLayer)) where create = return $ Layer 0


---------------------
-- === Network === --
---------------------

-- == Definitions === --

data Net (ls :: [*])
newtype NetLayer t a = NetLayer a deriving (Show, Functor, Traversable, Foldable)
newtype NetRef     a = NetRef   a deriving (Show, Functor, Traversable, Foldable)


-- === Instances === --

-- Type relations
type instance LayerData (NetLayer t a) = LayerData a

-- Layout
type TermShell ls term rec = (NetLayer term <$> ls) :| rec
type instance Layout2 (Net ls) fmt dyn sel = TermShell ls (Term2 (Net ls) fmt dyn sel) (TermRecord2 (Net ls) fmt dyn sel Int) -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)

-- Shell
type instance Shell.Access l (Term2 (Net ls) fmt dyn sel) = NetLayer (Term2 (Net ls) fmt dyn sel) l

-- Ref
type instance Referred (Term2 (Net ls) fmt dyn sel) = NetRef (Term2 (Net ls) fmt dyn sel)
instance Monad m => Referable m (Term2 (Net ls) fmt dyn sel) where refer' = return ∘ NetRef




----------------------------------



type TRex2 t fmt dyn sel = TermRecord2 t fmt dyn sel Int -- Int is a mock for parameterized binding (i.e. Link between nodes in Network)




a1 = () ^. from exprArgs :: Expr Blank' Static Int
-- -- t1 = Record.cons a1 :: AnyTerm SNet Draft Static
t1 = Record.cons a1 :: TRex2 t Draft Static 'Nothing

t2 = runIdentity $ overbuild t1 :: AnyTerm (Net '[IntLayer]) Draft Static

t3 = runIdentity $ refer t2

-- t4 :: OverBuilder Identity (Term2 t fmt dyn sel) => a -> Ref2 (Term2 t fmt dyn sel)
-- t4 a = runIdentity $ foox a
--
--
-- type Foox = Record.Variants (RecordOf (AnyTerm SNet Draft Static))


instance (Creator m c, OverBuilder m a) => OverBuilder m (Cover c a) where
    overbuild a = Cover <$> create <*> overbuild a

type ElemBuilder2 atom m a = (Record.Cons atom (RecordOf a), OverBuilder m a, Referable m a)

elemBuilder2 :: ElemBuilder2 atom m a => atom -> m (Ref2 a)
elemBuilder2 a = overbuild (Record.cons a) >>= refer

-- powinnismy wprowadzic abstrakcje Bind, ktora moglaby miec source i target i w networku reprezentowalaby connection

--
-- class Monad m => OverBuilder m a where
--     overbuild :: RecordOf a -> m a

-- tx = Record.cons Atom.Blank :: Term Int Draft Runtime.Dynamic

main :: IO ()
main = do
    E.main
    (_,  g :: NetGraph ) <- prebuild2
    renderAndOpen [ ("xg", "xg", g)
                  ]
    main2

main2 = do

    print t2
    print $ t2 ^. Shell.access' IntLayer



    -- caseTest tx $ do
    --     of' $ \Blank -> print "it is Blank!"
        -- of' $ \ANY        -> print "hello"

    caseTest t2 $ do
        of' $ \Blank -> print "it is Blank!"
    --     -- of' $ \ANY        -> print "hello"
