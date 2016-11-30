{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilyDependencies    #-}
{-# LANGUAGE RecursiveDo               #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UndecidableSuperClasses   #-} -- Only for symbol mapping, to be refactored
{-# LANGUAGE NoOverloadedStrings       #-} -- https://ghc.haskell.org/trac/ghc/ticket/12797

-- {-# LANGUAGE PartialTypeSignatures     #-}

module Main where


import Prologue            hiding (String, Simple, elem, typeRep, elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter, set, Setter', set')
import qualified Prologue as P

import           Control.Monad.Event     as Event
import qualified Control.Monad.Delayed    as Delayed
import           Control.Monad.Delayed    (Runner, MonadDelayed, Delayed, delayed)
import qualified Control.Monad.Writer     as Writer
import           Data.Construction        hiding (Register, register)
import           Data.Container           (index_)
import           Data.Container           hiding (impossible, elems, elems', elemsM)
-- import           Data.Layer_OLD.Cover_OLD
import qualified Data.Map                 as Map
-- import           Data.Version.Semantic
import           Development.Placeholders
import           Text.Printf              (printf)
import           Type.Inference

import           Data.Container.Hetero                           (Hetero(..), Any(..))
import           Luna.Pass.Inference.Calling         (FunctionCallingPass (..))
import           Luna.Pass.Inference.Importing       (SymbolImportingPass (..))
import qualified Luna.Pass.Inference.Importing       as Importing
import           Luna.Pass.Inference.Literals        (LiteralsPass (..))
import           Luna.Pass.Inference.Scan            (ScanPass (..))
import           Luna.Pass.Inference.Struct          (StructuralInferencePass (..))
import           Luna.Pass.Utils.Literals            as LiteralsUtils
import qualified Luna.Env.Env                                 as Env
import qualified Luna.IR.Library.Symbol                             as Symbol
import           Luna.IR.Term.Format


import qualified Data.RTuple.Examples as E
import qualified Data.RTuple as List
import           Data.RTuple (TMap(..), empty, Assoc(..)) -- refactor empty to another library



-- import Data.Shell as Shell hiding (Layers)
import Data.Cover
import Type.Applicative
import Luna.IR.Term hiding (Data, cons, unify, star, Readable, string, var)

-- import GHC.Prim (Any)

import Type.Promotion    (KnownNats, natVals)
import qualified Luna.IR.Internal.IR as IR
import Luna.IR.Internal.IR hiding (Bind, Fields, (:=))

import Prelude (error, undefined)
import Type.List (In)
import Data.Container.Hetero (Elems)
import GHC.TypeLits hiding (Symbol)
import GHC.TypeLits (ErrorMessage(Text))
import Luna.IR.Term.Atom (Atoms)

import qualified Luna.IR.Term.Symbol as Symbol
import qualified Luna.IR.Term.Symbol.Named as Sym
import qualified Luna.IR.Term.Symbol.Named as Symbol
import Luna.IR.Term.Symbol.Named (HasName, name)
import Luna.IR.Term.Symbol (Sym)
import Data.Property
import Luna.IR.Term.Format (Format, Sub)
import qualified Data.Vector as V
import qualified GHC.Prim as Prim
import Luna.IR.Term.Layout

import Unsafe.Coerce (unsafeCoerce)
import Type.Set as Set hiding (Set)
import qualified Type.List as TList
import qualified Control.Monad.State as State
import Control.Monad.State hiding (get, set, modify', modify)

import System.Exit (exitSuccess)

import Control.Monad.ST
import Data.Reprx
import Luna.IR.Repr.Styles  (HeaderOnly(..))
import Data.Aeson (ToJSON, FromJSON, encode, decode)
import qualified Control.Monad.State.Dependent.Old as D
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Web.Browser (openBrowser)

import qualified Luna.Diag.Vis as Vis
import           Luna.Diag.Vis (MonadVis)
import qualified Data.Set as S
import Type.Container (Every)
import Luna.IR.Layer
import Luna.IR.Layer.Model

import qualified Luna.Pass.Class as Pass
import Luna.Pass.Class (Keys, Preserves, Pass, Inputs, Outputs)
import Luna.IR.Layer.UID (UID, ID)
import qualified Luna.IR.Layer.UID as UID
import Luna.IR.Layer.Succs
import Luna.IR.Layer.Type
import qualified Luna.IR.Internal.LayerStore as Store
import Type.Maybe (FromJust)
import Luna.IR.Term.Layout.Ant














type family   UnsafeGeneralizable a b :: Constraint
type instance UnsafeGeneralizable (Term l) (Term l') = ()

unsafeGeneralize :: UnsafeGeneralizable a b => a -> b
unsafeGeneralize = unsafeCoerce ; {-# INLINE unsafeGeneralize #-}





type AntTerm a n t = Term (Ant a n t)
type UntyppedTerm a n = AntTerm a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


type Layouted l = KnownTypeT Layout (DefaultLayout l)
layouted :: forall l m a. Layouted l m a -> m a
layouted = baseLayout @(DefaultLayout l)



-- instance Generalize (Compound Simple lst) Draft




-- FIXME: moze po prostu Term Star?
newtype MagicStar = MagicStar Term'

makeWrapped ''MagicStar

newMagicStar :: IRMonad m => m MagicStar
newMagicStar = wrap' <$> magicTerm Sym.uncheckedStar ; {-# INLINE newMagicStar #-}

magicStar :: Iso' (Term l) MagicStar
magicStar = iso (wrap' . unsafeCoerce) (unsafeCoerce . unwrap') ; {-# INLINE magicStar #-}



snapshot :: (IRMonad m, MonadVis m, Readables m '[TermLayer UID, TermLayer Type, TermLayer Model, TermLinkLayer UID, TermLinkLayer Model, TermNet])
         => P.String -> m ()
snapshot title = do
    ts  <- terms
    vss <- mapM visNode2 ts
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    Vis.addStep (fromString title) vns ves


visNode2 :: (IRMonad m, Readables m '[TermLayer UID, TermLayer Type, TermLayer Model, TermLinkLayer UID, TermLinkLayer Model])
         => Term' -> m (Vis.Node, [Vis.Edge])
visNode2 t = do
    euid   <- readLayer @UID   t
    tpLink <- readLayer @Type  t
    tpUid  <- readLayer @UID   tpLink
    (l,r)  <- readLayer @Model tpLink
    lUID   <- readLayer @UID   l
    rUID   <- readLayer @UID   r
    ins    <- symbolFields t
    let header = fromString $ "foo" -- reprStyled HeaderOnly t
        node   = Vis.Node (fromString "") euid euid (fromList [header])
        tpVis  = if lUID == rUID then [] else [Vis.Edge (fromString "") tpUid tpUid lUID rUID (fromList [fromString "type"])]
        mkEdge (i,l,r) = Vis.Edge (fromString "") i i l r mempty
        getUIDs e = do
            i      <- readLayer @UID   e
            (l, r) <- readLayer @Model e
            lUID   <- readLayer @UID   l
            rUID   <- readLayer @UID   r
            return (i, lUID, rUID)

    uss <- mapM getUIDs ins

    let edges = tpVis <> (mkEdge <$> uss)
    return (node, edges)



consTypeLayer :: IRMonad m
              => Store.STRefM m (Maybe MagicStar) -> Term t -> Definition (Term t) -> m (LayerData Type (Term t))
consTypeLayer ref self _ = do
    top  <- view (from magicStar) <$> localTop ref
    conn <- magicLink top self
    return conn


localTop :: IRMonad m
         => Store.STRefM m (Maybe MagicStar) -> m MagicStar
localTop ref = Store.readSTRef ref >>= \case
    Just t  -> return t
    Nothing -> mdo
        Store.writeSTRef ref $ Just s
        s <- newMagicStar
        Store.writeSTRef ref Nothing
        return s


-- TODO[WD]: dont allow here to use registerGenericLayer!
--           maybe LayerConsPasses will help here?
layerReg4 :: IRMonad m => m ()
layerReg4 = registerElemLayer @TERM @Type . consTypeLayer =<< runInIR (Store.newSTRef Nothing)




runRegs :: IRMonad m => m ()
runRegs = do
    runElemRegs
    runLayerRegs

-- === Elem reg defs === --

elemRegs :: IRMonad m => [m ()]
elemRegs = [elemReg1, elemReg2]

runElemRegs :: IRMonad m => m ()
runElemRegs = sequence_ elemRegs

elemReg1 :: IRMonad m => m ()
elemReg1 = registerElem @TERM

elemReg2 :: IRMonad m => m ()
elemReg2 = registerElem @(LINK' TERM)


-- === Layer reg defs === --

layerRegs :: IRMonad m => [m ()]
layerRegs = [layerReg1, layerReg2, layerReg3, layerReg4]

runLayerRegs :: IRMonad m => m ()
runLayerRegs = sequence_ layerRegs


layerReg1 :: IRMonad m => m ()
layerReg1 = registerGenericLayer @Model $ \ _ -> return

layerReg2 :: IRMonad m => m ()
layerReg2 = registerGenericLayer @Succs $ \ _ _ -> return def



consUIDLayer :: PrimMonad m => Store.STRefM m ID -> t -> Definition t -> m (LayerData UID t)
consUIDLayer ref _ _ = Store.modifySTRef' ref (\i -> (i, succ i))

layerReg3 :: IRMonad m => m ()
layerReg3 = registerGenericLayer @UID . consUIDLayer =<< runInIR (Store.newSTRef 0)



type family AsSubLayout t l
type instance AsSubLayout Name (Ant a n t) = a <+> n


type family LiteralLayout t layout
type family AtomLayout    t layout

type instance LiteralLayout p (Ant a n t) = Ant p () Star
type instance AtomLayout    p (Ant a n t) = Ant p () Star

-- Specialized
type instance Specialized Atom s (Ant a n t) = Ant (Simplify (AsSubLayout Atom s :> a)) n t
type instance Specialized Name s (Ant a n t) = Ant a (Simplify (AsSubLayout Name s :> n)) t
type instance Specialized Type s (Ant a n t) = Ant a n (Simplify (AsSubLayout Type s :> t))

-- type Ant  l a n t = Compound l '[Atom := a, Name := n, Type := t]
-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------

type l <+> r = Merge              l r
type l |>  r = Specialized   Atom l r
type l #>  r = Specialized   Name l r
type l >>  r = Specialized   Type l r
type l %>  r = LiteralLayout      l r

type instance Simplify (a :> ()) = a

type family   DefListLayout (m :: * -> *) a
type instance DefListLayout m P.String = String %> Infered Layout m
type instance DefListLayout m (Term t) = t

class                                         LitTerm m a        where litTerm :: a -> m (Term (DefListLayout m a))
instance (IRMonad m, Accessible TermNet m) => LitTerm m P.String where litTerm = string ; {-# INLINE litTerm #-}
instance Monad m                           => LitTerm m (Term l) where litTerm = return ; {-# INLINE litTerm #-}


var :: (IRMonad m, Accessibles m '[TermNet, TermLinkNet], Inferable2 Layout ldef m, LitTerm m name)
    => name -> m (Term (DefListLayout m name #> AtomLayout Var ldef))
var name = mdo
    t <- term $ Sym.uncheckedVar l
    n <- litTerm name
    l <- link (unsafeGeneralize n) t
    return t

string :: (IRMonad m, Accessible TermNet m) => P.String -> m (Term (String %> Infered Layout m))
string = term . uncheckedString ; {-# INLINE string #-}

star :: (IRMonad m, Accessible TermNet m, Inferable2 Layout ldef m) => m (Term (AtomLayout Star ldef))
star = term Sym.uncheckedStar
{-# INLINE star #-}

unify :: (IRMonad m, Accessibles m '[TermNet, TermLinkNet])
      => Term l -> Term l' -> m (Term (Unify |> (l <+> l')))
unify a b = mdo
    t  <- term $ Sym.uncheckedUnify la lb
    la <- link (unsafeGeneralize a) t
    lb <- link (unsafeGeneralize b) t
    return t
{-# INLINE unify #-}


data MyData = MyData Int deriving (Show)


data                    SimpleAA
type instance Inputs    SimpleAA = '[Attr MyData, TermNet, TermLinkNet] <> TermLayers '[Model, UID, Type] <> TermLinkLayers '[Model, UID]
type instance Outputs   SimpleAA = '[Attr MyData, TermNet, TermLinkNet] <> TermLayers '[Model, UID, Type] <> TermLinkLayers '[Model, UID]
type instance Preserves SimpleAA = '[]

pass1 :: (MonadFix m, MonadIO m, IRMonad m, MonadVis m) => Pass SimpleAA m
pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m, MonadVis m) => m (Either Pass.Err ())
test_pass1 = runIRT $ do
    runRegs

    attachLayer (typeRep @Model) (typeRep @TERM)
    attachLayer (typeRep @Succs) (typeRep @TERM)
    attachLayer (typeRep @Type)  (typeRep @TERM)
    attachLayer (typeRep @UID)   (typeRep @TERM)

    attachLayer (typeRep @Model) (typeRep @(LINK' TERM))
    attachLayer (typeRep @UID)   (typeRep @(LINK' TERM))

    setAttr $ MyData 7

    Pass.eval pass1



type family Head a

type instance Access Atom (Ant a _ _) = a
type instance Head (Atomic a) = Atomic a

type AtomHead l = Head (l # Atom)
type AtomHeadDef l = TermSymbolDef (AtomHead l) (Term l)

-- match' :: forall a l m. (IRMonad m, Readable (TermLayer Model) m, Atomic a ~ AtomHead l) => Term l -> m (TermSymbolDef (Atomic a) (Term l))
-- match' = unsafeToTermSymbolDef @(Atomic a)

type KnownAtom l m = (IRMonad m, Readable (TermLayer Model) m) -- CheckAtomic (AtomHead l))

match' :: forall l m. KnownAtom l m => Term l -> m (AtomHeadDef l)
match' = unsafeToTermSymbolDef @(AtomHead l)


source :: (IRMonad m, Readable (Layer (Abstract (Link a b)) Model) m) => Link a b -> m a
source = fmap fst . readLayer @Model ; {-# INLINE source #-}

gen_pass1 :: ( MonadIO m, IRMonad m, MonadVis m
             , Accessibles m '[TermLayer Model, TermLinkLayer Model, TermLayer Type, TermLinkLayer UID, TermLayer UID, TermNet, TermLinkNet, Attr MyData]
             ) => m ()
gen_pass1 = layouted @ANT $ do
    (s1 :: Term (Ant Star   ()     Star)) <- star
    (s2 :: Term (Ant Star   ()     Star)) <- star
    snapshot "s1"
    (n  :: Term (Ant String ()     Star)) <- string "hello"
    (v  :: Term (Ant Var    String Star)) <- var n
    (v2 :: Term (Ant Var    String Star)) <- var "foo"
    snapshot "s2"
    u1 <- unify s1 s2
    snapshot "s3"
    print "hello"
    d <- readLayer @Type u1
    print d
    md <- readAttr @MyData
    print md
    ts <- terms
    print ts

    match s1 $ \case
        Unify l r -> print "ppp"
        Star      -> match s1 $ \case
            Unify l r -> print "hola"
            Star      -> print "hellox"

    print "---"


    match v $ \ (Var l) -> do
        n <- source l
        match n $ \case
            String s -> print s

    v <- var "ala"
    n <- strName v
    print n


    return ()

-- strName :: _ => _
strName v = getName v >>= \n -> match' n >>= \ (Sym.Sym_String s) -> return s

type KnownName l m = (KnownAtom l m, HasName (AtomHeadDef l), Readable (TermLinkLayer Model) m)

getName :: KnownName l m => Term l -> m (Term (Sub Name l))
getName v = match' v >>= \ vv -> source (vv ^. name)


main :: IO ()
main = do
    -- test_g4
    (p, vis) <- Vis.newRunDiffT test_pass1
    case p of
        Left e -> do
            print "* INTERNAL ERROR *"
            print e
        Right _ -> do
            let cfg = ByteString.unpack $ encode $ vis
            -- putStrLn cfg
            liftIO $ openBrowser ("http://localhost:8200?cfg=" <> cfg)
            return ()
    print p
    return ()
