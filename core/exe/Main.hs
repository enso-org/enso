{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# BOOSTER  VariantCase               #-}

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Data.Graph          hiding (Dynamic, Connection)
import Data.Graph.Builders
import Prologue            hiding (Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type)

import           Control.Monad.Event
import qualified Control.Monad.Writer     as Writer
import           Old.Data.Attr                (attr)
import           Data.Construction
import           Data.Container           (elems, index_)
import           Data.Container           hiding (impossible)
import           Data.Graph.Builder       hiding (get)
import           Data.Graph.Query         hiding (Graph)
import qualified Data.Graph.Query         as Sort
import           Data.Index               (idx)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
import           Old.Data.Prop
import           Data.Record              hiding (Cons, Layout, cons, Value)
import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import           Data.Container.Hetero                           (Hetero(..), Any(..))
import qualified Data.Graph.Builder.Class                        as Graph
import qualified Data.Graph.Builder.Class                        as Graph.Builder
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
import           Luna.Syntax.Model.Network.Builder.Node          hiding (curry, star, star2, blank, unify)
import qualified Luna.Syntax.Model.Network.Builder.Node          as Old
import           Luna.Syntax.Model.Network.Builder.Node.Class    ()
import qualified Luna.Syntax.Model.Network.Builder.Node.Inferred as Inf
import           Luna.Syntax.Model.Network.Builder.Term.Class    (NetCluster, NetGraph, NetGraph2, NetLayers, NetNode, fmapInputs, inputstmp, runNetworkBuilderT, runNetworkBuilderT2)
import           Luna.Syntax.Model.Network.Class                 (Network)
import qualified Luna.Syntax.Model.Network.Term                  as Net
-- import           Luna.Syntax.Term                               (OverBuilder, Layout_OLD, ExprRecord, overbuild, AnyExpr)
import qualified Old.Luna.Syntax.Term.Class                           as Term
import           Luna.Syntax.Term.Expr.Format

import qualified Data.Graph.Backend.NEC       as NEC
import           Data.Graph.Model.Pointer.Set (RefSet)

import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty, Assoc(..)) -- refactor empty to another library

import           Luna.Syntax.Model.Network.Builder.Class ()
import qualified Luna.Syntax.Model.Network.Builder.Class as XP
import           Luna.Syntax.Model.Network.Builder.Term.Class (star2, ExprBuilder)

import qualified Data.Record                  as Record
import qualified Data.Graph.Builder                      as GraphBuilder

import Data.Shell as Shell hiding (Layers)
import Data.Cover
import Type.Applicative
import Luna.Syntax.Term.Expr hiding (Data)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.Syntax.Term.Expr.Class as TEST
import Luna.Syntax.Term.Expr.Class hiding (Bind, Fields, Layer, (:=)) -- (Model, Name, All, cons2, Layout(..), Term, Term3, Data(Data), Network2, NetworkT, consTerm, unsafeConsTerm, term, Term2)
import Data.Record.Model.Masked (encodeStore, encodeData2, Store2, Slot(Slot), Enum, Raw, Mask)

import Luna.Syntax.Model.Network.Builder.Term.Class (TermBuilder)
import Prelude (error, undefined)
import Type.List (In)
import Data.Container.Hetero (Elems)
import GHC.TypeLits hiding (Symbol)
import GHC.TypeLits (ErrorMessage(Text))
import Luna.Syntax.Term.Expr.Atom (Atoms)

import qualified Luna.Syntax.Term.Expr.Symbol as Symbol
import qualified Luna.Syntax.Term.Expr.Symbol.Named as N
import qualified Luna.Syntax.Term.Expr.Symbol.Named as Symbol
import qualified Luna.Syntax.Term.Expr.Symbol2 as S2
import Luna.Syntax.Term.Expr.Symbol (Sym)
import Control.Lens.Property
import Luna.Syntax.Term.Expr.Format (Format, Sub)
import TH
import qualified Data.Vector as V
import qualified GHC.Prim as Prim
import qualified Luna.Syntax.Term.Expr.Layout as Layout

import Unsafe.Coerce (unsafeCoerce)
import Type.Set as Set hiding (Set)
import qualified Type.List as TList

title s = putStrLn $ "\n" <> "-- " <> s <> " --"






runCase :: Term t layers model -> [Prim.Any -> out] -> out
runCase el ftable = ($ s) $ flip V.unsafeIndex idx $ V.fromList ftable where
    s   = unwrap' $ get @Sym $ unwrap' $ get @Data el
    idx = unwrap' $ get @Atom $ unwrap' $ get @Data el
{-# INLINE runCase #-}


matchx f = f . unsafeCoerce
{-# INLINE matchx #-}


defaultMatch = error "wrong match"
{-# INLINE defaultMatch #-}

--



type Network3 m = NEC.HMGraph (PrimState m) '[Node, Edge, Cluster]




type family LookupAssoc k s where
    LookupAssoc k '[]            = 'Nothing
    LookupAssoc k (k ':= v ': _) = 'Just v
    LookupAssoc k (l ':= _ ': s) = LookupAssoc k s




type family MatchModels m1 m2
type family UniScope t1 t2

type instance MatchModels (Layout.Named n1 t1) (Layout.Named n2 t2) = Layout.Named (UniScope n1 n2) (UniScope t1 t2)




type family MatchXScopes a b
type instance MatchXScopes Draft Draft = Draft
type instance MatchXScopes Draft Value = Draft


type instance MatchModels (Layout.TNA t n a) (Layout.TNA t' n' a') = Layout.TNA (UniScope t t') (UniScope n n') (UniScope a a')


type instance UniScope Draft Draft = Draft
type instance UniScope Value Draft = Draft


                -- xunify :: forall t a m model model1 model2 layout n x.
                --           ( t ~ NetworkT a, Monad m, model ~ MatchModels model1 model2, TEST.ASTBuilder t m
                --           , TEST.MatchModel (Symbol Unify layout) t model, layout ~ Layout.Named n x)
                --         => Ref Edge (Term2 t model1) -> Ref Edge (Term2 t model2) -> m (Term2 t model)
                -- xunify l r = consTerm $ N.unify (unsafeCoerce l) (unsafeCoerce r)

-- moze zakodowac glebiej zaleznosci - w Symbolach ?
-- moznaby pisac wtedy np.
-- data    instance Symbol Acc      layout = Acc     !(Bind Name layout) !(Bind Child layout)
-- lub cos podobnego, przycyzm layout musialby zawierac `sys` !

data Type
data SimpleX



----------------------------
-- === Uniform layout === --
----------------------------

-- === Definitions === --

data Uniform a


-- === Utils === --

type Universal a = Set Model (Uniform Draft) a

universal :: a -> Universal a
universal = unsafeCoerce ; {-# INLINE universal #-}



-------------------------
-- === Prim layout === --
-------------------------

-- === Definition === --

data Prim name atom


-- === Isntances === --

type instance Get Atom (Prim _ atom) = atom
type instance Get Name (Prim name _) = name
type instance Get Type (Prim _ _)    = Star



-----------------------------
-- === Compound layout === --
-----------------------------

-- === Definition === --

data Compound t (ls :: [Assoc * *])


-- === Instances === --

type instance Get p (Compound t ls) = Get p ls

type instance MatchModels (Compound t bs) (Compound t bs') = Compound t (MatchByKeys (Set.ToList (Concat (AsSet (List.Keys bs)) (AsSet (List.Keys bs')))) bs bs')



-------------------------------
-- === Hierarchy layouts === --
-------------------------------

-- === Definition === --
-- TODO: refactor, it exists in Luna.Syntax.Term.Expr.Format

-- === Instances === --

type instance Sub t (a :> b) = b
type instance Atoms (a :> b) = Atoms a










type ANTLayout l a n t = Compound l '[Atom := a, Name := n, Type := t]




data Net = Net


type instance Get p   (Ref2 t a) = Get p a
type instance Set p v (Ref2 t a) = Ref2 t (Set p v a)





type instance Sub Atom (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Atom a) n t
type instance Sub Name (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Name n) (Sub Name n) (Sub Name n)
type instance Sub Type (ANTLayout SimpleX a n t) = ANTLayout SimpleX (Sub Type t) (Sub Type t) (Sub Type t)


type family MatchByKeys (ks :: [*]) (bs :: [Assoc * *]) (bs' :: [Assoc * *]) :: [Assoc * *] where
    MatchByKeys '[] bs bs' = '[]
    MatchByKeys (k ': ks) bs bs' = (k ':= MatchFinal (LookupAssoc k bs) (LookupAssoc k bs')) ': MatchByKeys ks bs bs'

type family MatchFinal l r where
    MatchFinal 'Nothing  ('Just a)  = a
    MatchFinal ('Just a) 'Nothing   = a
    MatchFinal ('Just a) ('Just a') = MatchModels a a'


type instance MatchModels (Form a) (Form a) = Form a
-- type instance MatchModels Value Value = Value
-- type instance MatchModels Draft Draft = Draft





type family ExtendModel c l t
-- type instance ExtendModel c (ExprX layers a n t) (ExprX layers a' n' t') = Set Model (ExtendModel c (ExprX layers a n t ^. Model) (ExprX layers a' n' t' ^. Model)) (ExprX layers a n t)

type instance ExtendModel Atom (ANTLayout SimpleX a n t) (ANTLayout SimpleX a' n' t') = ANTLayout SimpleX (Merge a a') (Merge n n') (Merge t t')
type instance ExtendModel Name (ANTLayout SimpleX a n t) (ANTLayout SimpleX a' n' t') = ANTLayout SimpleX a (Merges '[n, a', n', t']) t
type instance ExtendModel Type (ANTLayout SimpleX a n t) (ANTLayout SimpleX a' n' t') = ANTLayout SimpleX a n (Merges '[t, a', n', t'])

type family Merges lst where
    Merges '[a]      = a
    Merges (a ': as) = Merge a (Merges as)

type family Specialized t spec model

-- type instance Specialized p spec (ExprX layers a n t) = Set Model (Specialized p spec (ExprX layers a n t ^. Model)) (ExprX layers a n t)
type instance Specialized Atom spec (ANTLayout l a n t) = ANTLayout l (Simplify (spec :> a)) n t








star_auto2 :: (DefaultModel Star model, TermCons Star m layers model) => m (Term t layers model)
star_auto2 = term2 N.star'

star_auto3 :: (DefaultModel Star model, LayersCons layers m, ValidateModel model Atom Star) => m (Term t layers model)
star_auto3 = term2 N.star'

star_auto4 :: LayersCons layers m => m (PrimTerm' t layers Star)
star_auto4 = term2 N.star'



type TermBuilder2 t layers m = (LayersCons layers m, Bindable Node m (AnyTerm t layers), Dispatcher2 Node m (Binding Node (AnyTerm t layers)))
-- type AtomBuilder2 t model    = (DefaultModel t model, ValidateModel model Atom t)

star_auto6 :: TermBuilder2 t layers m => m $ Binding Node (PrimTerm' t layers Star)
star_auto6 = universalDispatch Node =<< universalBind3 =<< star_auto4


-- type AnyExpr ls = Term3 (ExprX2 ls (Uniform Draft))

type PrimTerm  t ls name atom = Term t ls (Prim name atom)
type PrimTerm' t ls      atom = PrimTerm t ls () atom
type AnyTerm  t ls           = Term t ls (Uniform Draft)


universalDispatch t a = a <$ dispatch t (universal a)




type family DefaultModel defAtom t :: Constraint
-- type instance DefaultModel defAtom (ExprX layers a n t) = DefaultModel defAtom (ExprX layers a n t ^. Model)
type instance DefaultModel defAtom (ANTLayout l a n t) = (a ~ defAtom, n ~ (), t ~ ())


-- TODO: rename vvv
type DefaultModel' a t = DefaultModel a (t ^. Model)



-- type instance DefaultModel (ANTLayout SimpleX a n t) = ANTLayout SimpleX () () ()



-- !!! Moze zamienic Generalizable na TF zwracajaca jakas wartosc lub Constraint?
class Generalizable a b
-- instance Generalizable t t' => Generalizable (Term3 t) (Term3 t')
-- instance (layers ~ layers', Generalizable model model') => Generalizable (ExprX2 layers model) (ExprX2 layers' model')
instance Generalizable a (Uniform Draft)

generalize :: Generalizable a b => a -> b
generalize = unsafeCoerce ; {-# INLINE generalize #-}





-- instance validates ... => Generalizable (ExprX layers a n t) (ExprX layers a' n' t') where generalize = unsafeCoerce ; {-# INLINE generalize #-}


-- Universal powinno podmieniac model na UniversalModel. Z takim modelem mozemy wartosci wkaldac do grafu
-- po wlozeniu wartosci do grafu mozemy je odczytywac i przetwarzac, bo znamy ich Typy. Mozemy tez
-- je rzutowac na inne modele, ktore pokrywaja sie z Universal (czyli wszysktie polaczenia sa draftami)
-- Dzieki uzywaniu modelu Universal upraszczaja sie nam typy, np. Dowolny Expr przeniesiony na Universalma model
-- UniversalModel i nie musi byc robiona mapa po Assocs szczegolnie jezeli bedizemy chieli supportowac nieznane klucze
-- Dodatkowo, trzeba przejsc na DynamicM2, wtedy graf bedzie przechowywal informacje o wartosciach
-- i bedzie mozna go lepiej o nie odpytywac bez niebezpeicznego rzutowania, poniewaz wszystkie wartosci tam
-- beda w UniversalModel,,,,,,,

-- type family Universal a
-- type instance Universal I = I
--
-- type Universal' a = Set Model (Universal (a ^. Model)) a
--
-- universal :: Term3 t -> Universal (Term3 t)
-- universal = unsafeCoerce ; {-# INLINE universal #-}
--
--
-- type instance Universal (Compound l as) = Compound l (List.ReplaceVals Draft as)
-- type instance Universal (ExprX ls a n t) = Universal' (ExprX ls a n t)


-- class Bindable3 t m a where
--     bind3 :: a -> m (Connector a t a)

type instance Connector Net = Ref2

class Monad m => Bindable t m a where
    bind :: a -> m (Binding t a)

instance (MonadBuilder g m, DynamicM2 Node g m (Term Net layers model))
      => Bindable Node m (Term Net layers model) where
    bind a = Binding <$> construct' a







universalBind2 :: Constructor' m (Binding Node (AnyTerm t layers))
               => Term t layers model -> m (Binding Node (Term t layers model))
universalBind2 = unsafeUniversalAppM construct'

universalBind3 :: Bindable Node m (AnyTerm t layers)
               => Term t layers model -> m (Binding Node (Term t layers model))
universalBind3 = unsafeUniversalAppM bind


unsafeUniversalAppM :: Functor m => (Universal a -> m (Universal b)) -> a -> m b
unsafeUniversalAppM f a = unsafeCoerce <$> (f $ universal a)

class IsExprX a
instance (v ~ Binding Node (Term t layers model), layers ~ '[]) => IsExprX v
type IsExprX' = TypeConstraint2 IsExprX


instance (Wrapped (Binding t b), Unwrapped (Binding t b) ~ ref a, Referred ref m)
      => MonadAccess (Binding t b) m a where
    write2 = refer   . unwrap'
    read2  = derefer . unwrap'


test_gr1 :: (TermBuilder2 t layers m, Referred (Connector t Node) m)
         => m $ Binding Node (PrimTerm' t layers Star)
test_gr1 = do
    sref <- star_auto6
    t <- read2 sref
    -- print "!!!"
    -- print sref
    -- print t
    return sref


test_g2 :: forall m . (PrimMonad m, MonadIO m)
        => m (Binding Node (PrimTerm' Net '[] Star), Network3 m)
test_g2 = do
    g <- NEC.emptyHMGraph
    flip Graph.Builder.runT g $ suppressAll
                              $ runListener @Node @IsExprX'
                              $ test_gr1


main :: IO ()
main = do
    -- print blank
    print N.blank
    -- print $ (runIdentity (encodeStore blank) :: Store2 '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ])
    print $ (runIdentity (encodeStore N.blank) :: Store2 '[ Atom ':= Enum, Format ':= Mask, Sym ':= Raw ])
    -- let e1  = (runIdentity (cons2 blank  ) :: Term Network2 '[] '[Int] (Layout Static Draft) (Layout Static Draft))
    -- let e1   = (runIdentity (cons2 N.blank) :: Term Network2 '[] '[Int] (Layout N.String Draft) (Layout N.String Draft))
    -- let e1'  = (runIdentity (cons2 N.blank) :: Term2 Network2 '[] '[Int] (Layout.Named N.String Draft))
    let -- e1  = (runIdentity (cons2 N.blank) :: Term Network2 (Layout.Named N.String Draft))
        -- er1 = (runIdentity (cons2 N.blank) :: Ref Edge (Term Network2 (Layout.Named Draft Draft)))
        -- u1  = (runIdentity (unify_x er1 er1) :: Ref Edge (Term Network2 (Layout.Named Draft Draft)))

        -- xe1 = (runIdentity (consTerm N.blank) :: (Term2 Network2 (Layout.TNA Draft Draft Draft)))
        --
        -- fs1 = Ptr 0 :: Ref Edge (Term2 (NetworkT a) (Layout.TNA Draft Draft Value))
        -- fs2 = Ptr 0 :: Ref Edge (Term2 (NetworkT a) (Layout.TNA Draft Draft Draft))

        -- s1 = S2.star :: S2.Symbol Star Net

        -- ss1 = runIdentity (term N.blank') :: MyExpr2 '[] Draft Draft Draft


        -- fss1 = 0 :: Ref2 Edge (MyExpr2 '[] (Missing :> Draft) Draft Draft)
        -- fss2 = 0 :: Ref2 Edge (MyExpr2 '[] (App :> Draft) Draft Draft)

        x1 = runIdentity star_auto3 :: Term Net '[] (ANTLayout SimpleX Star () ())
        -- su1 = runIdentity (term $ N.unify' fss1 fss1) :: MyExpr2 '[] (Unify :> Value) Draft Draft

        -- uux = runIdentity $ unify_auto fss2 fss1 :: Int

        -- fu1 = runIdentity $ xunify fs1 fs2 :: Int


    print "==="
    (x,g) <- test_g2
    print x
    -- print g
    -- let e2  = (runIdentity (cons2 blank  ) :: Expr' Static Draft)
    -- let e2 = (runIdentity (cons2 N.blank) :: Expr' Static Draft)
    -- let es1 = (runIdentity (cons2 star) :: Ref Edge (Expr' Static Value))
    -- let eb1 = (runIdentity (cons2 blank) :: Ref Edge (Expr' Static Draft))
    -- let eu1 = (runIdentity (cons2 $ unify eb1 eb1) :: Expr' Static Draft)
    -- let eu2 = (runIdentity (cons2 $ unify es1 es1) :: Expr Static Value Static Draft)
    -- print e2

    -- print s1
    putStrLn ""
    print x1
    print $ get @Data x1
    print $ get @Sym $ unwrap' $ get @Data x1
    print $ unwrap' $ get @Atom $ unwrap' $ get @Data x1
    --
    -- (a,g) <- test_g2
    -- print a

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


    -- let xx = (let blank = 2 in blank)

    --
    let { exp = x1
    ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
        ala = 11
    ;f2 = matchx $ \Symbol.Star       -> (print "hello2" )
       where ala = 11
    } in $(testTH2 'exp [ [p|Symbol.Unify l r|], [p|Symbol.Star|] ] ['f1, 'f2])
            --
            -- -- let { exp = e1
            -- -- ;f1 = matchx $ \(Symbol.Unify l r) -> print ala where
            -- --     ala = 11
            -- -- } in $(testTH2 'exp [ [p|Symbol.Unify l r|] ] ['f1])
            --
    case' x1 of
        Symbol.Unify l r -> print 11
        Symbol.Star      -> case' x1 of
            Symbol.Unify l r -> print "hello"
            Symbol.Star      -> print "hello3x"


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
