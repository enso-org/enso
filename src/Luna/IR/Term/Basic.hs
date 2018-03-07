{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE GADTs      #-}

module Luna.IR.Term.Basic where

import Prologue               hiding (cast)

import Foreign.Ptr            (Ptr, castPtr, plusPtr)
import Foreign.Storable       (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)
import Foreign.Storable.Utils (sizeOf', alignment', castPtrTo, intPtr)
import qualified Foreign.Storable as Storable
import qualified Foreign.Storable1 as Storable1

import qualified Foreign            as Ptr
import qualified Data.Graph as Graph
import qualified Foreign.Memory.Pool as MemPool

import Foreign.Storable.Deriving  (deriveStorable)
import Foreign.Storable1.Deriving (deriveStorable1)

import Luna.IR.Class
import OCI.IR.Term
-- import qualified OCI.IR.Layout as Layout
import OCI.IR.Link as Link

import qualified Luna.IR.Link as Link
import Luna.IR.Format
import Data.Tag (Tag, TagOf)

import qualified Data.Tag     as Tag
import qualified Data.Mutable as MData
import Control.Monad.State.Layered (get, put)
import qualified Control.Monad.State.Layered as State
import OCI.IR.Component
import OCI.IR.Conversion
import OCI.IR.Selector

import qualified Data.TypeMap.Strict as TypeMap
import qualified Data.Tuple.Strict as Tuple

import  OCI.IR.Layout ((:=))
import qualified OCI.IR.Layout as Layout

-- import Control.Monad.State.Strict hiding (return, liftIO, MonadIO)

import Type.Data.Ord (Cmp)
import Foreign.Marshal.Alloc (mallocBytes)

import Foreign.Ptr.Utils (SomePtr)

import OCI.Pass.Class as Pass

import qualified Data.Map                    as Map

import Luna.IR.Term.TH

import qualified OCI.Pass.Manager as PassManager
import           OCI.Pass.Manager (MonadPassManager)

import qualified Control.Monad.Exception as Exception
import Type.Data.Bool

-- import OCI.IR.Layer (Layer)
import qualified OCI.IR.Layer as Layer
import qualified OCI.IR.Layer.Internal as Layer
-- import qualified OCI.IR.Layer2 as Layer2


type family SizeOf a :: Nat
type instance SizeOf Int = 8 -- FIXME: support 32 bit platforms too!


data Model


----------------
-- === IR === --
----------------

-- === IR Atoms === ---

type family TermConsDef t :: Type -> Type

Tag.familyInstance "TermCons" "Var"
newtype ConsVar a = Var
    { __name :: Int
    } deriving (Show, Eq)
type instance TermConsDef Var = ConsVar
type instance Layer.ConsLayout ConsVar = Var
deriveStorable ''ConsVar
deriveStorable1 ''ConsVar
deriveLinks ''ConsVar

Tag.familyInstance "TermCons" "Acc"
data ConsAcc a = Acc
    { __base :: !(Link (Layout.Get Terms a :-: Layout.Set Model Acc a)) -- !(Link.Term Acc a)
    , __name :: !(Link (Layout.Get Terms a :-: Layout.Set Model Acc a)) -- !(Link.Name Acc a)
    } deriving (Show, Eq)
type instance TermConsDef Acc = ConsAcc
type instance Layer.ConsLayout ConsAcc = Acc
deriveStorable ''ConsAcc
deriveStorable1 ''ConsAcc
deriveLinks ''ConsAcc

data UniTerm a
    = UniTermVar !(ConsVar a)
    | UniTermAcc !(ConsAcc a)
    deriving (Show, Eq)
deriveStorable ''UniTerm
deriveStorable1 ''UniTerm
deriveLinks ''UniTerm

class IsTermCons t where
    toUniTerm :: ∀ a. t a -> UniTerm a

instance IsTermCons ConsVar where toUniTerm = UniTermVar ; {-# INLINE toUniTerm #-}
instance IsTermCons ConsAcc where toUniTerm = UniTermAcc ; {-# INLINE toUniTerm #-}

-- instance Storable1.Storable1 ConsVar where
--     sizeOf    = Storable.sizeOf    ; {-# INLINE sizeOf    #-}
--     alignment = Storable.alignment ; {-# INLINE alignment #-}
--     peek      = Storable.peek      ; {-# INLINE peek      #-}
--     poke      = Storable.poke      ; {-# INLINE poke      #-}
--
-- instance Storable1.Storable1 ConsAcc where
--     sizeOf    = Storable.sizeOf    ; {-# INLINE sizeOf    #-}
--     alignment = Storable.alignment ; {-# INLINE alignment #-}
--     peek      = Storable.peek      ; {-# INLINE peek      #-}
--     poke      = Storable.poke      ; {-# INLINE poke      #-}
--
-- instance Storable1.Storable1 UniTerm where
--     sizeOf    = Storable.sizeOf    ; {-# INLINE sizeOf    #-}
--     alignment = Storable.alignment ; {-# INLINE alignment #-}
--     peek      = Storable.peek      ; {-# INLINE peek      #-}
--     poke      = Storable.poke      ; {-# INLINE poke      #-}



-- xchunkSize = sizeOf' @Int
--
-- instance Storable (ConsVar a) where
--     sizeOf    _ = xchunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = xchunkSize     ; {-# INLINE alignment #-}
--     peek ptr = Var <$> peek (intPtr ptr) ; {-# INLINE peek #-}
--     poke ptr (Var !a) = poke (intPtr ptr) a ; {-# INLINE poke #-}
--
-- instance Storable (ConsAcc a) where
--     sizeOf    _ = 2 * xchunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = xchunkSize     ; {-# INLINE alignment #-}
--     peek ptr = Acc <$> peek (castPtr ptr) <*> peekByteOff ptr xchunkSize ; {-# INLINE peek #-}
--     poke ptr (Acc !a !b) = poke (castPtr ptr) a >> pokeByteOff ptr xchunkSize b ; {-# INLINE poke #-}
--
-- instance Storable (UniTerm a) where
--     sizeOf    _ = 3 * xchunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = xchunkSize     ; {-# INLINE alignment #-}
--     peek ptr = peek (intPtr ptr) >>= \case
--         0 -> UniTermVar <$> peekByteOff ptr xchunkSize
--         1 -> UniTermAcc <$> peekByteOff ptr xchunkSize
--         _ -> error "Unrecognized constructor"
--     {-# INLINE peek #-}
--     poke ptr = \case
--         UniTermVar !a -> poke (intPtr ptr) 0 >> pokeByteOff ptr xchunkSize a
--         UniTermAcc !a -> poke (intPtr ptr) 1 >> pokeByteOff ptr xchunkSize a
--     {-# INLINE poke #-}


            -- peek        :: ∀ a.   Ptr (t a)               -> IO (t a)
            -- poke        :: ∀ a.   Ptr (t a)        -> t a -> IO ()
--
-- x :: Term '[ Model := Draft -< '[Model := Value]
--            , Type  := Value
--            ]
--
-- match x of
--     Acc l r -> ... (l :: Link )

-- defineTermConses [|
-- data Var a = Var { name :: Int                     }
-- data Acc a = Acc { base :: Link a , name :: Link a }
-- |]


-- Model, Argument, Model, Name ...
--
-- Type, Model,


-- type instance TermConsOf (Tag t a) = TermConsDef (Tag t a) (Tag t a)



newtype LayerLoc a = LayerLoc {_byteOffset :: Int } deriving (Show)
makeLenses ''LayerLoc



-- readLayer  :: forall layer t layout m. (KnownLayer t layer, Layer t layer layout, MonadIO m) => Component t layout -> m (Layer.View t layer layout)
-- writeLayer :: forall layer t layout m. (KnownLayer t layer, Layer t layer layout, MonadIO m) => Component t layout ->   (Layer.View t layer layout) -> m ()
-- readLayer  = Layer.unsafeReadByteOff  @layer (layerOffset @t @layer) ; {-# INLINE readLayer  #-}
-- writeLayer = Layer.unsafeWriteByteOff @layer (layerOffset @t @layer) ; {-# INLINE writeLayer #-}

-- Component comp layout -> m (LayerData comp layer layout)

-- -------------------------------------
-- -- === Global Layer Management === --
-- -------------------------------------
--
-- -- === Layer registry === --
--
-- type family AllLayers component :: [Type]
--
--
-- -- === LayerOffset === --
--
-- type LayerOffset component layer = LayerOffset' 0 component layer (AllLayers component)
-- type family LayerOffset' off component layer ls where
--     LayerOffset' s _ l (l ': _)  = s
--     LayerOffset' s t l (p ': ps) = LayerOffset' (s + LayerSize t p) t l ps
--
-- type KnownLayer component layer = KnownType (LayerOffset component layer)
-- layerOffset :: forall component layer. (KnownLayer component layer) => Int
-- layerOffset = fromInteger $ fromType @(LayerOffset component layer) ; {-# INLINE layerOffset #-}
--
--
-- -- === TotalLayersSize === --
--
-- type TotalLayersSize component = TotalLayersSize' component 0 (AllLayers component)
-- type family TotalLayersSize' t s ls where
--     TotalLayersSize' _ s '[] = s
--     TotalLayersSize' t s (l ': ls) = TotalLayersSize' t (s + LayerSize t l) ls
--
-- type KnownLayers component = KnownType (TotalLayersSize component)
-- totalLayersSize :: forall component. KnownLayers component => Int
-- totalLayersSize = fromInteger $ fromType @(TotalLayersSize component) ; {-# INLINE totalLayersSize #-}
--
--



-- type instance LayerSize Terms Model = 3 * SizeOf Int

-- instance Layer Terms Model where

-- type instance Layer.View Terms Model (Format   f) = UniTerm
-- type instance Layer.View Terms Model (TermCons f) = TermConsDef (TermCons f)

type instance Layer.Data   Terms XType = Link
-- type instance Layer.Layout Terms XType layout = layout *-* Layout.Get XType layout
type instance Layer.Layout Terms XType layout = layout *-* layout


type instance Layer.Layout   Terms Model layout = layout
type instance Layer.Data     Terms Model = UniTerm
type instance Layer.ConsData Terms Model (TermCons f) = TermConsDef (TermCons f)
-- type instance Layer     Terms Type   =
-- type instance Layer.View Terms Type a =
-- instance StorableLayer.View Terms Model (Format f)
-- instance Storable (Layer.View' Terms Model (TermCons f))
--     => StorableLayer.View Terms Model (TermCons f) where
--     peekLayer.ViewIO ptr = peek (ptr `plusPtr` constructorSize) ; {-# INLINE peekLayer.ViewIO #-}


type instance Layer.Layout Links Source layout = Layout.Get Source layout
type instance Layer.Layout Links Target layout = Layout.Get Target layout
type instance Layer.Data   Links Source        = Term
type instance Layer.Data   Links Target        = Term
-- type instance Layer.View Links Source a = Term
-- type instance Layer.View Links Target a = Term

-- instance StorableLayer.View Links Source a
-- instance StorableLayer.View Links Target a






-- type instance AllLayers Terms = '[Model]
-- type instance AllLayers Links = '[Model]




-- type instance Layer.View Model (Tag t a) = TermCons (Tag t a)

-- class LayerStorable layer (layout :: Layout) where
--     type family Layer.View layer layout :: *
--     layerByteSize   :: Int
--     layerByteOffset :: Int
--
-- instance LayerStorable Model



chunkSize :: Int
chunkSize = sizeOf' @Int ; {-# INLINE chunkSize #-}

-- instance Storable (UniTerm fmt a) where
--     sizeOf    _ = 3 * chunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = chunkSize     ; {-# INLINE alignment #-}
--     peek ptr = peek (intPtr ptr) >>= \case
--         0 -> Var <$> peekByteOff ptr chunkSize
--         1 -> Acc <$> peekByteOff ptr chunkSize <*> peekByteOff ptr (chunkSize*2)
--         _ -> error "Unrecognized constructor"
--     {-# INLINE peek #-}
--     poke ptr = \case
--         Var !a    -> poke (intPtr ptr) 0 >> pokeByteOff ptr chunkSize a
--         Acc !a !b -> poke (intPtr ptr) 1 >> pokeByteOff ptr (chunkSize*2) b
--     {-# INLINE poke #-}





-- class Reader layer where

mockNewIR :: MonadIO m => m (Term Draft)
mockNewIR = Component . coerce <$> MemPool.allocPtr @(UniTerm ()) ; {-# INLINE mockNewIR #-}

mockNewLink :: forall m. MonadIO m => m (Link (Draft :-: Draft))
mockNewLink = undefined -- Component . coerce <$> MemPool.alloc @(LinkData Draft Draft)



type IRAllocator comp m = (MonadIO m, Pass.ComponentMemPoolGetter comp m)

-- | Its unsafe because it only allocates raw memory.
noInitAllocIR :: ∀ comp m layout. IRAllocator comp m => m (Component comp layout)
noInitAllocIR = wrap <$> (MemPool.alloc =<< Pass.getComponentMemPool @comp) ; {-# INLINE noInitAllocIR #-}



term  :: (IRAllocator Terms m, Layer.Writer Terms Model m, IsTermCons t) => t a -> m (Term (Layout.Set Model (Layer.ConsLayout t) a))
term' :: (IRAllocator Terms m, Layer.Writer Terms Model m, IsTermCons t) => t a -> m (Term b)
term = term' ; {-# INLINE term #-}
term' term = do
    ir <- noInitAllocIR
    Layer.write @Model ir (toUniTerm term)
    return $ cast ir
{-# INLINE term' #-}


var :: (IRAllocator Terms m, Layer.Writer Terms Model m) => Int -> m (Term Var)
var name = term' $ Var name ; {-# INLINE var #-}


link :: (IRAllocator Links m, Layer.Writer Links Source m, Layer.Writer Links Target m) => Term src -> Term tgt -> m (Link (src *-* tgt))
link src tgt = do
    ir <- noInitAllocIR @Links
    Layer.write @Source ir src
    Layer.write @Target ir tgt
    return $ ir
{-# INLINE link #-}


type src *-* tgt = Layout.FromList [Source := src, Target := tgt]

-- Tag.familyInstance "TermCons" "Var"
-- newtype ConsVar a = Var
--     { __name :: Int
--     } deriving (Show, Eq)
-- type instance TermConsDef Var = ConsVar
-- type instance Layer.ConsLayout ConsVar = Var
-- deriveStorable ''ConsVar
-- deriveLinks ''ConsVar

-- newComponent :: forall t a m. (KnownLayers t, MonadIO m) => m (Component t a)
-- newComponent = Component . coerce <$> MemPool.allocBytes (totalLayersSize @t)


-- termMemPool, linkMemPool :: MemPool
-- termMemPool = unsafePerfromIO newMemPool ; {-# NOINLINE termMemPool #-}
-- linkMemPool = unsafePerfromIO newMemPool ; {-# NOINLINE linkMemPool #-}
--
--

data XType

-- type instance Layout.DefLayout layout (TermCons t) = Draft

data MyPass
type instance Spec MyPass t = Spec_MyPass t
type family   Spec_MyPass t where
    Spec_MyPass (In Elems) = '[Terms, Links]
    Spec_MyPass (In Terms) = '[Model, XType]
    Spec_MyPass (In Links) = '[Source, Target]
    Spec_MyPass (Out a)    = Spec_MyPass (In a)
    Spec_MyPass t          = '[]


test_pm_run :: MonadIO m => m Pass.PassConfig
test_pm_run = Exception.catchAll undefined $ PassManager.evalT test_pm

test_pm :: (MonadPassManager m, MonadIO m) => m Pass.PassConfig
test_pm = do
    PassManager.registerComponent @Terms
    PassManager.registerPrimLayer @Terms @Model
    PassManager.registerPrimLayer @Terms @XType

    PassManager.registerComponent @Links
    PassManager.registerPrimLayer @Links @Source
    PassManager.registerPrimLayer @Links @Target

    reg <- State.get @PassManager.Registry
    passCfg <- PassManager.mkPassConfig reg

    return passCfg


passTest :: Pass.Pass MyPass
passTest = do
    v1 <- var 5
    v2 <- var 7
    v3 <- var 9
    l1 <- link v1 v2

    Layer.write @XType v1 l1

    s <- Layer.read @Source l1
    m <- Layer.read @Model s
    print m
    return ()

passTest_run :: IO ()
passTest_run = do

    cfg <- test_pm_run
    xx <- Pass.encodePassState cfg
    Pass.runPass xx passTest

--
-- passRunTest :: IO ()
-- passRunTest = do
--     mp <- MemPool.new 100
--     let cfg = Pass.PassConfig
--             $ Map.insert (someTypeRep @Terms)
--               (Pass.ComponentConfig 7
--                   ( Map.insert (someTypeRep @Model)
--                     (Pass.LayerConfig 11)
--                   $ mempty
--                   ) mp
--               )
--             $ mempty
--
--     xx <- Pass.encodePassState cfg
--     Pass.runPass xx passTest
-- runPass :: Functor m => PassState pass -> SubPass pass m a -> m a





--
-- type instance Pass.In AnyLayer Terms MyPass = '[Model]


layerLoc0 :: Int
layerLoc0 = 0 ; {-# INLINE layerLoc0 #-}

test :: IO ()
test = do
    var1 <- mockNewIR
    var2 <- mockNewIR
    acc1 <- mockNewIR
    Layer.unsafeWriteByteOff @Model layerLoc0 var1 (UniTermVar $ Var 7)
    Layer.unsafeWriteByteOff @Model layerLoc0 var2 (UniTermVar $ Var 5)

    -- l1 <- mockNewLink
    -- Layer.unsafeWriteByteOff @Model layerLoc0 l1 (LinkData var1 acc1)

    -- Layer.unsafeWriteByteOff @Model layerLoc0 acc1 (UniTermAcc $ Acc l1 l1)

    -- print var1
    -- x <- Layer.unsafeReadByteOff @Model layerLoc0 var1
    -- print x




test_readWriteLayer :: Int -> IO ()
test_readWriteLayer i = do
    !ir <- mockNewIR
    Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
    let go 0 = return ()
        go j = do
            UniTermVar (Var !y) <- Layer.unsafeReadByteOff @Model layerLoc0 ir
            UniTermVar (Var !z) <- Layer.unsafeReadByteOff @Model layerLoc0 ir
            UniTermVar (Var !x) <- Layer.unsafeReadByteOff @Model layerLoc0 ir
            Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var $! x+1)
            go $! (j - 1)
    go i
    -- Ptr.free ptr

--
-- test_readWriteLayer2 :: Int -> IO ()
-- test_readWriteLayer2 i = do
--     ir <- mockNewIR
--     writeLayer layerLoc0 ir (UniTermVar $ Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go 0 = return ()
--         go j = do
--             set <- get'
--             let layer = TypeSet.unsafeLookup @(LayerLoc Model) set
--             Var x <- readLayer layer ir
--             writeLayer layer ir (Var (x+1))
--             go (j - 1)
--     State.evalT (go i)
--         $ TypeSet.insert (XInt 1)
--         $ TypeSet.insert (6 :: Int)
--         $ TypeSet.insert layerLoc0
--         $ mempty



test_readWriteLayer_ptrOff :: Int -> IO ()
test_readWriteLayer_ptrOff i = do
    -- ptr <- mallocBytes (sizeOf' @Int * _MAX_LAYERS)
    ptr <- Ptr.new (0 :: Int)
    ir <- mockNewIR
    Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
    let -- go :: Int -> StateT Int IO ()
        go 0 = return ()
        go j = do
            -- set <- get'
            layer <- peek ptr
            UniTermVar (Var x) <- Layer.unsafeReadByteOff @Model layer ir
            Layer.unsafeWriteByteOff @Model layer ir (UniTermVar $ Var (x+1))
            go (j - 1)
    (go i)

_MAX_LAYERS :: Int
_MAX_LAYERS = 16

test_readWriteLayer_ptrBuffOff :: Int -> IO ()
test_readWriteLayer_ptrBuffOff i = do
    (ptr :: Ptr Int) <- mallocBytes (sizeOf' @Int * _MAX_LAYERS)
    poke ptr 0
    pokeByteOff ptr (sizeOf' @Int) (0 :: Int)
    pokeByteOff ptr (sizeOf' @Int * 2) (0 :: Int)
    pokeByteOff ptr (sizeOf' @Int * 3) (0 :: Int)
    pokeByteOff ptr (sizeOf' @Int * 4) (0 :: Int)
    pokeByteOff ptr (sizeOf' @Int * 5) (0 :: Int)
    pokeByteOff ptr (sizeOf' @Int * 6) (0 :: Int)
    pokeByteOff ptr (sizeOf' @Int * 7) (0 :: Int)
    ir <- mockNewIR
    Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
    let -- go :: Int -> StateT Int IO ()
        go 0 = return ()
        go j = do
            p <- get @(Ptr Int)
            x <- get @Int
            put @Int (x+1)
            layer <- liftIO $ peek p
            UniTermVar (Var x) <- Layer.unsafeReadByteOff @Model layer ir
            Layer.unsafeWriteByteOff @Model layer ir (UniTermVar $ Var (x+1))
            go (j - 1)
    flip State.evalT (0::Int)
       $ flip State.evalT ptr (go i)

-- test_readWriteLayer_static :: Int -> IO ()
-- test_readWriteLayer_static i = do
--     ir <- mockNewIR
--     writeLayer @Model ir (UniTermVar $ Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go 0 = return ()
--         go j = do
--             UniTermVar (Var x) <- readLayer @Model ir
--             writeLayer @Model ir (UniTermVar $ Var (x+1))
--             go (j - 1)
--     (go i)

    -- State.evalT (go i)
    --     $ TypeSet.insert (XInt 1)
    --     $ TypeSet.insert (6 :: Int)
    --     $ TypeSet.insert layerLoc0
    --     $ mempty

    -- Ptr.free ptr

--
-- readWritePtr :: Int -> IO ()
-- readWritePtr i = do
--     ptr <- Ptr.new (0 :: Int)
--     let go 0 = return ()
--         go j = do
--             x <- peek ptr
--             poke ptr (x+1)
--             go (j - 1)
--     go i
--     Ptr.free ptr




test_readWriteLayer2 :: Int -> IO ()
test_readWriteLayer2 i = do
    ir <- mockNewIR
    Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
    let -- go :: Int -> StateT Int IO ()
        go 0 = return ()
        go j = do
            !set <- State.get'
            let !layer = TypeMap.getElem @Int set
            UniTermVar (Var !x) <- Layer.unsafeReadByteOff @Model layer ir
            Layer.unsafeWriteByteOff @Model layer ir (UniTermVar $ Var $! (x+1))
            go (j - 1)
    State.evalT (go i) (TypeMap.TypeMap (Tuple.T3 ('x' :: Char) ("a" :: String) (0 :: Int)) :: TypeMap.TypeMap '[Char, String, Int])
    -- Ptr.free ptr

-- test_readWriteLayer3 :: Int -> IO ()
-- test_readWriteLayer3 i = do
--     ir <- mockNewIR
--     Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go :: Int -> Pass.Pass MyPass
--         go 0 = return ()
--         go j = do
--             !s <- getPassState
--             Pass.LayerByteOffset !layer <- Pass.getData @(Pass.LayerByteOffset Terms Model)
--             UniTermVar (Var !x) <- Layer.unsafeReadByteOff @Model layer ir
--             Layer.unsafeWriteByteOff @Model layer ir (UniTermVar $ Var $! (x+1))
--             go (j - 1)
--
--     mp <- MemPool.new 100
--     let cfg = Pass.PassConfig
--             $ Map.insert (someTypeRep @Terms)
--               (Pass.ComponentConfig 0
--                   ( Map.insert (someTypeRep @Model)
--                     (Pass.LayerConfig 0)
--                   $ mempty
--                   ) mp
--               )
--             $ mempty
--     xx <- Pass.encodePassState cfg
--     Pass.runPass xx (go i)
    -- State.evalT (go i) (TypeMap.TypeMap (Tuple.T1 (0 :: Int)) :: TypeMap.TypeMap '[Int])


test_readWriteLayer4 :: Int -> IO ()
test_readWriteLayer4 i = do
    ir <- mockNewIR
    Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
    let go :: Int -> Pass.Pass MyPass
        go 0 = return ()
        go j = do
            UniTermVar (Var !x) <- Layer.read @Model ir
            Layer.write @Model ir (UniTermVar $ Var $! (x+1))
            go (j - 1)

    cfg <- test_pm_run
    xx <- Pass.encodePassState cfg
    Pass.runPass xx (go i)
    -- State.evalT (go i) (TypeMap.TypeMap (Tuple.T1 (0 :: Int)) :: TypeMap.TypeMap '[Int])

-- type instance Layout.GetBase Var = Var
tttest :: Term Var -> Pass.Pass MyPass
tttest n = do
    -- (x :: _) <- Layer.read @Model n
    -- (x :: _) <- Layer.readCons__ @Terms @Model n
    return ()
-- unsafeReadByteOff  :: CTX => Int -> Component comp layout -> m (LayoutView comp layer layout)
-- #define CTX ∀ layer comp layout m. (StorableLayer comp layer layout, MonadIO m)

-- readLayer :: ∀ layer comp layout m. (Layer.StorableLayer comp layer layout, MonadIO m, Pass.PassDataGetter (Pass.LayerByteOffset comp layer) m)
--           => Component comp layout -> m (Layer.LayoutView comp layer layout)
-- readLayer comp = do
--     Pass.LayerByteOffset !off <- Pass.getData @(Pass.LayerByteOffset comp layer)
--     Layer.unsafeReadByteOff @layer off comp
-- {-# INLINE readLayer #-}
-- Pass.LayerByteOffset !layer <- Pass.getData @(Pass.LayerByteOffset Terms Model)
-- UniTermVar (Var !x) <- Layer.unsafeReadByteOff @Model layer ir








-- passTest :: Pass.Pass MyPass
-- passTest = do
--     s <- getPassState
--     print s
--     print =<< Pass.getData @(Pass.LayerByteOffset Terms Model)
--     return ()
--
-- passRunTest :: IO ()
-- passRunTest = Pass.runPass (Pass.encodePassStateTEMP layout) passTest where
--     layout = Pass.PassConfig
--         $ Map.insert (someTypeRep @Terms)
--           (Pass.ComponentConfig 7
--               $ Map.insert (someTypeRep @Model)
--                 (Pass.LayerConfig 11)
--               $ mempty
--           )
--         $ mempty
    -- Ptr.free ptr
-- test_readWriteLayer2 :: Int -> IO ()
-- test_readWriteLayer2 i = do
--     ir <- mockNewIR
--     Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go 0 = return ()
--         go j = do
--             layer <- State.get'
--             -- let LayerLoc layer = TypeSet.unsafeLookup @(LayerLoc Model) set
--             UniTermVar (Var x) <- Layer.unsafeReadByteOff @Model layer ir
--             Layer.unsafeWriteByteOff @Model layer ir (UniTermVar $ Var (x+1))
--             go (j - 1)
--     State.evalT (go i) (0 :: Int)
--     -- State.evalT (go i) (TypeSet.insert (LayerLoc 0 :: LayerLoc Model) mempty)
--     -- Ptr.free ptr


-- test_readWriteLayer2 :: Int -> IO ()
-- test_readWriteLayer2 i = do
--     ir <- mockNewIR
--     Layer.unsafeWriteByteOff @Model layerLoc0 ir (UniTermVar $ Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go 0 = return ()
--         go j = do
--             set <- State.get'
--             let LayerLoc layer = TypeSet.unsafeLookup @(LayerLoc Model) set
--             UniTermVar (Var x) <- Layer.unsafeReadByteOff @Model layer ir
--             Layer.unsafeWriteByteOff @Model layer ir (UniTermVar $ Var (x+1))
--             go (j - 1)
--     State.evalT (go i) (TypeSet.insert (LayerLoc 0 :: LayerLoc Model) mempty)
--     -- Ptr.free ptr

-- Layer.unsafeWriteByteOff :: forall layer t layout m. (Layer t layer layout, MonadIO m) =>
--   Int -> Component t layout ->   (Layer.View t layer layout) -> m ()

-- readIO @Model

-- layerLoc0 = Layer (sizeOf' @)
-- newtype IRDef (layers :: [Type]) = IRDef (Ptr ()) deriving (Show)


-- newtype IR a = IR (IRDef '[])
-- class Layer l where



-- var :: Int -> IR Var



-- foo = do
--     let x = undefined :: Term Var
--         y = x
--         y :: Int
--     return ()



-- newtype TermConsDefVar (a :: Layout) = TermConsDefVar
--     { __name :: Int
--     } deriving (Show, Eq)
-- type instance TermConsDef Var a = TermConsDefVar a

-- data IRDefVar (a :: Layout) = IRDefVar
--     { __type :: {-# UNPACK #-} !(Link.Type Var a)
--     , __term :: {-# UNPACK #-} !(TermConsDefVar a)
--     }




-- data TermConsDefAcc a = TermConsDefAcc
--     { __base :: !(Link.Term Acc a)
--     , __name :: !(Link.Name Acc a)
--     } deriving (Show, Eq)
-- type instance TermConsDef Acc a = TermConsDefAcc a

-- vvv - moze nie trzeba takich datatypow jezeli trzymalibysmy to w pamieci i skakli poitnerem robiac read @LayerName ?x
-- data IRDefAcc (a :: Layout) = IRDefAcc
--     { __type :: {-# UNPACK #-} !(Link.Type Acc a)
--     , __term :: {-# UNPACK #-} !(TermConsDefAcc a)
--     }

-- data family Term t (a :: Layout)
--
-- data instance Term (TermCons t) a = Foo Int



-- type Term (l :: Layout) = TermConsDef (Base l) l


-- read @Term :: IR Draft -> IR.Term Draft
-- read @Type :: IR Draft -> IR.Link Draft -- autofollow?


-- class Reader layer where
--     readIO :: forall layout. IR layout -> IO (Layer.View SubLayout layer)

--
-- foo = do
--     let x :: Term Acc
--         x = undefined
--         y = x
--         y :: Int
--     return ()

-- data IR a = IR
--     { __tp   :: !(Link.Type )
--     , __term ::
--     }

-- data family TermX (t :: *) (a :: Layout)
--
-- data instance TermX Acc a = TermConsDefAcc
--     { __base :: !(Link.Term Acc a)
--     , __name :: !(Link.Name Acc a)
--     } deriving (Show, Eq)


-- data Foo a = Foo {-# UNPACK #-} !(TermX a)
--
-- data Term a
--     = Var !Int
--     | Acc !(IRLinkRef a) !(IRLinkRef a)
--     deriving (Generic, Show, Eq)
--
--
            -- read :: IRRef Draft -> IR Draft
            -- term :: IR Draft -> Term Draft
            -- tp   :: IR Draft -> Link.Type Draft Draft
            --
            -- readLayer @Type :: IRRef Draft -> Layer Type -- read only needed bits!
            --
            --
            -- read :: IRRef Var -> IR Var
            -- term :: IR Var -> Term Var
            -- tp   :: IR Var -> Link.Type Var Draft
            --
            -- jezeli odczytywanie bitow w czunkach jest tak szamo szybkie to moze zmergowac
            -- IRRef i IR ?
            -- po prostu
            -- read @Term zwracaloby tak samo dane jak
            -- read @Type ?
            --
            -- pattern amtche mozna tez tak :
            --
            -- case x of
            --     (Var, v) ->
            --     (Acc, a) ->
            --
            -- ale to nie jest ladne, bo nie pozwala na proste pattern matche pol

-- n <- read nref


-- -- -- === Instances === --
--
-- chunkSize :: Int
-- chunkSize = sizeOf' @Int
--
-- instance Storable a => Storable (Core a) where
--     sizeOf    _ = 3 * chunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = chunkSize     ; {-# INLINE alignment #-}
--     peek ptr = peek (intPtr ptr) >>= \case
--         0 -> UVar <$> peekByteOff ptr chunkSize
--         1 -> UAcc <$> peekByteOff ptr chunkSize
--         _ -> error "Unrecognized constructor"
--     {-# INLINE peek #-}
--     poke ptr = \case
--         UVar !a -> poke (intPtr ptr) 0 >> pokeByteOff ptr chunkSize a
--         UAcc !a -> poke (intPtr ptr) 1 >> pokeByteOff ptr chunkSize a
--     {-# INLINE poke #-}
--
-- instance Storable a => Storable (Acc a) where
--     sizeOf    _ = 2 * chunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = chunkSize     ; {-# INLINE alignment #-}
--     peek ptr = Acc <$> peek (castPtr ptr) <*> peekByteOff ptr chunkSize ; {-# INLINE peek #-}
--     poke ptr = \(Acc !b !n) -> poke (castPtr ptr) b >> pokeByteOff ptr chunkSize n ; {-# INLINE poke #-}
--
-- instance Storable a => Storable (Var a) where
--     sizeOf    _ = chunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = chunkSize ; {-# INLINE alignment #-}
--     peek ptr = Var <$> peek (castPtr ptr)        ; {-# INLINE peek #-}
--     poke ptr = \(Var !n) -> poke (castPtr ptr) n ; {-# INLINE poke #-}
--
--
--
--





-- x :: IRRef Draft
--
-- read n >>= \case
--     Var n ->
--
-- TermDraft
--
--
--
-- instance Storable a => Storable (Edge a) where
--     sizeOf    _ = 3 * chunkSize ; {-# INLINE sizeOf    #-}
--     alignment _ = chunkSize     ; {-# INLINE alignment #-}
--     peek ptr = peek (intPtr ptr) >>= \case
--         0 -> UVar <$> peekByteOff ptr chunkSize
--         1 -> UAcc <$> peekByteOff ptr chunkSize
--         _ -> error "Unrecognized constructor"
--     {-# INLINE peek #-}
--     poke ptr = \case
--         UVar !a -> poke (intPtr ptr) 0 >> pokeByteOff ptr chunkSize a
--         UAcc !a -> poke (intPtr ptr) 1 >> pokeByteOff ptr chunkSize a
--     {-# INLINE poke #-}



--
--
--
-- -- ---------------------------
-- -- -- === Testing utils === --
-- -- ---------------------------
-- --
-- -- mkSampleData :: Int -> Int -> Core ()
-- -- mkSampleData i j = UAcc $ Acc (Edge (EdgeID i)) (Edge (EdgeID j))
-- --
-- -- fromSampleData :: Core () -> Int
-- -- fromSampleData (UAcc (Acc (Edge (EdgeID i)) _)) = i
-- --
-- --
-- --
-- -- newtype Spec a = Spec a deriving (Show, Functor, Foldable, Traversable)
-- -- makeLenses ''Spec
-- --
-- --
-- -- instance Storable a => Storable (Spec a) where
-- --     sizeOf    _ = sizeOf' @a + 1                   ; {-# INLINE sizeOf    #-}
-- --     alignment _ = alignment' @a                    ; {-# INLINE alignment #-}
-- --     peek      p = coerce <$> peek @a (castPtr p)   ; {-# INLINE peek      #-}
-- --     poke      p a = poke @a (castPtr p) (coerce a) ; {-# INLINE poke      #-}
--
--
-- -- -------------------------
-- -- -- === Strict List === --
-- -- -------------------------
-- -- -- NOTE[piotrMocz]: Could alternatively use Data.List.Strict
-- --
-- -- data List = Cons {-# UNPACK #-} !Int List | Null deriving (Show) -- TODO: Why making strict spine makes it so slow to generate? With lazy one, even if we use all the elements, the whole process is shorter than generating it with strict spine.
-- --
-- -- instance Mempty    List where mempty = Null ; {-# INLINE mempty  #-}
-- -- instance Semigroup List where
-- --     l <> r = case l of
-- --         Null     -> r
-- --         Cons a t -> Cons a (t <> r)
-- --     {-# INLINE (<>) #-}
-- --
-- -- instance IsList List where
-- --     type Item List = Int
-- --     toList   = \case
-- --         Null      -> []
-- --         Cons a as -> a : toList as
-- --     fromList x = case x of
-- --         (a:as) -> Cons a $ fromList as
-- --         []     -> Null
-- --     {-# INLINE toList   #-}
-- --     {-# INLINE fromList #-}




--
--
-- data IRTYPE
-- data IRType a
--
-- data LITERAL
-- data VALUE
-- data THUNK
-- data PHRASE
-- data DRAFT
--
-- type Literal = IRType LITERAL
-- type Value   = IRType VALUE
-- type Thunk   = IRType THUNK
-- type Phrase  = IRType PHRASE
-- type Draft   = IRType DRAFT
--
-- data Format
--    = Literal
--    | Value
--    | Thunk
--    | Phrase
--    | Draft
--
--
-- -- data family IR (t :: Format)
