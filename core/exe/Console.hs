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


import Prologue            hiding (Simple, elem, typeRep, elements, Symbol, Cons, Num, Version, cons, read, ( # ), Enum, Type, Getter, set, Setter', set')
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
import Luna.IR.Term hiding (Data, cons, unify, star, Readable)

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





type AntTerm a n t = Term (Ant' a n t)
type UntyppedTerm a n = AntTerm a n Star

baseLayout :: forall t m a. KnownTypeT Layout t m a -> m a
baseLayout = runInferenceT2 @Layout


type Layouted l = KnownTypeT Layout (DefaultLayout l)
layouted :: forall l m a. Layouted l m a -> m a
layouted = baseLayout @(DefaultLayout l)



instance Generalize (Compound Simple lst) Draft




-- FIXME: moze po prostu Term Star?
newtype MagicStar = MagicStar Term'

makeWrapped ''MagicStar

newMagicStar :: IRMonad m => m MagicStar
newMagicStar = wrap' <$> magicTerm Sym.uncheckedStar ; {-# INLINE newMagicStar #-}

magicStar :: Iso' (Term l) MagicStar
magicStar = iso (wrap' . unsafeCoerce) (unsafeCoerce . unwrap') ; {-# INLINE magicStar #-}



snapshot :: (IRMonad m, Readables m '[TermLayer UID, TermLayer Type, TermLayer Model, TermLinkLayer UID, TermLinkLayer Model, TermNet])
         => P.String -> m ()
snapshot title = do
    ts  <- terms
    vss <- mapM visNode2 ts
    let vns = fst <$> vss
        ves = join $ snd <$> vss
    return ()
    -- Vis.addStep (fromString title) vns ves


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






-----------------------------------------------------------------------
-----------------------------------------------------------------------
-----------------------------------------------------------------------


star :: (IRMonad m, Accessible TermNet m, Inferable2 Layout layout m) => m (AtomicTerm Star layout)
star = term Sym.uncheckedStar
{-# INLINE star #-}

unify :: (IRMonad m, Accessibles m '[TermNet, TermLinkNet])
      => Term l -> Term l' -> m (Term (Unify :>> (l <+> l')))
unify a b = mdo
    n  <- term $ Sym.uncheckedUnify la lb
    la <- link (unsafeGeneralize a) n
    lb <- link (unsafeGeneralize b) n
    return n
{-# INLINE unify #-}


data MyData = MyData Int deriving (Show)


data                    SimpleAA
type instance Inputs    SimpleAA = '[Attr MyData, TermNet, TermLinkNet] <> TermLayers '[Model, UID, Type]
type instance Outputs   SimpleAA = '[Attr MyData, TermNet, TermLinkNet] <> TermLayers '[Model, UID, Type]
type instance Preserves SimpleAA = '[]

pass1 :: (MonadFix m, MonadIO m, IRMonad m) => Pass SimpleAA m
pass1 = gen_pass1

test_pass1 :: (MonadIO m, MonadFix m, PrimMonad m) => m (Either Pass.Err ())
test_pass1 = runIRT $ do
    runRegs

    attachLayer (typeRep @Model) (typeRep @TERM)
    attachLayer (typeRep @Succs) (typeRep @TERM)
    attachLayer (typeRep @Type)  (typeRep @TERM)
    attachLayer (typeRep @UID)   (typeRep @TERM)
    setAttr $ MyData 7

    Pass.eval pass1


gen_pass1 :: ( MonadIO m, IRMonad m
             , Accessibles m '[TermLayer Model, TermLayer Type, TermNet, TermLinkNet, Attr MyData]
             ) => m ()
gen_pass1 = layouted @ANT $ do
    (s1 :: UntyppedTerm Star ()) <- star
    (s2 :: UntyppedTerm Star ()) <- star
    u1 <- unify s1 s2
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

    return ()



main :: IO ()
main = do
    -- test_g4
    p <- test_pass1
    print p
    return ()
