{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeInType      #-}
{-# LANGUAGE TemplateHaskell      #-}

module Luna.IR.Term.Basic where

import Prologue
import Foreign.Ptr            (Ptr, castPtr, plusPtr)
import Foreign.Storable       (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)
import Foreign.Storable.Utils (sizeOf', alignment', castPtrTo, intPtr)

import qualified Foreign            as Ptr
import qualified Data.Graph as Graph
import qualified Foreign.Memory.Pool as MemPool

import Foreign.Storable.Deriving

import Luna.IR.Class
import OCI.IR.Term hiding (TermModelDef)
import qualified OCI.IR.Layout as Layout
import OCI.IR.Link as Link

import qualified Luna.IR.Link as Link
import Luna.IR.Format
import Data.Tag (Tag)

import qualified Data.Mutable as MData
import qualified Data.TypeSet as TypeSet
import Control.Monad.State.Layered hiding ((.))
-- import Control.Monad.State.Strict hiding (return, liftIO, MonadIO)

import Type.Data.Ord (Cmp)
import Foreign.Marshal.Alloc (mallocBytes)





type SomePtr = Ptr ()

type family SizeOf a :: Nat
type instance SizeOf Int = 8 -- FIXME: support 32 bit platforms too!





----------------
-- === IR === --
----------------

-- === IR Atoms === ---

type family TermModelDef t :: Type -> Type
-- data family TermModelDef (t :: *) (a :: Layout)


type family TermModel a


type Var = TermTag VAR; data VAR
newtype TermVar a = TermVar
    { __name :: Int
    } deriving (Show, Eq)
type instance TermModelDef Var = TermVar
deriveStorable ''TermVar

type Acc = TermTag ACC; data ACC
data TermAcc a = TermAcc
    { __base :: !(Link.Term Acc a)
    , __name :: !(Link.Name Acc a)
    } deriving (Show, Eq)
type instance TermModelDef Acc = TermAcc
deriveStorable ''TermAcc

data TermUni fmt a
    = Var !Int
    | Acc !(Link.Term fmt a) !(Link.Term fmt a)
    deriving (Show, Eq)
type instance TermModelDef (FormatTag f) = TermUni (FormatTag f)
deriveStorable ''TermUni




type instance TermModel (Tag t a) = TermModelDef (Tag t a) (Tag t a)


data Model

newtype LayerLoc a = LayerLoc {_byteOffset :: Int } deriving (Show)
makeLenses ''LayerLoc






-------------------
-- === Layer === --
-------------------

-- === Definition === --

type family LayerSize component layer :: Nat

class Storable (LayerData t layer cfg) => Layer (t :: Type) (layer :: Type) (cfg :: Type) where
    type family LayerData t layer cfg :: Type

    peekLayerIO :: SomePtr -> IO (LayerData t layer cfg)
    pokeLayerIO :: SomePtr -> LayerData t layer cfg -> IO ()
    initLayerIO :: SomePtr -> IO ()
    peekLayerIO !ptr = peek $ coerce ptr ; {-# INLINE peekLayerIO #-}
    pokeLayerIO !ptr = poke $ coerce ptr ; {-# INLINE pokeLayerIO #-}
    initLayerIO _ = return ()


-- === API === --

peekLayer :: forall t l cfg m. (Layer t l cfg, MonadIO m) => SomePtr -> m (LayerData t l cfg)
pokeLayer :: forall t l cfg m. (Layer t l cfg, MonadIO m) => SomePtr -> LayerData t l cfg -> m ()
peekLayer !ptr    = liftIO $ peekLayerIO @t @l @cfg ptr   ; {-# INLINE peekLayer #-}
pokeLayer !ptr !v = liftIO $ pokeLayerIO @t @l @cfg ptr v ; {-# INLINE pokeLayer #-}

peekLayerByteOff :: forall layer t cfg m. (Layer t layer cfg, MonadIO m) => Int -> SomePtr -> m (LayerData t layer cfg)
pokeLayerByteOff :: forall layer t cfg m. (Layer t layer cfg, MonadIO m) => Int -> SomePtr ->   (LayerData t layer cfg) -> m ()
peekLayerByteOff !off !ptr      = peekLayer @t @layer @cfg (ptr `plusPtr` off)     ; {-# INLINE peekLayerByteOff #-}
pokeLayerByteOff !off !ptr !val = pokeLayer @t @layer @cfg (ptr `plusPtr` off) val ; {-# INLINE pokeLayerByteOff #-}

unsafeReadLayerByteOff  :: forall layer t cfg m. (Layer t layer cfg, MonadIO m) => Int -> Component t cfg -> m (LayerData t layer cfg)
unsafeWriteLayerByteOff :: forall layer t cfg m. (Layer t layer cfg, MonadIO m) => Int -> Component t cfg ->   (LayerData t layer cfg) -> m ()
unsafeReadLayerByteOff  !off !t = peekLayerByteOff @layer @t @cfg off (coerce t) ; {-# INLINE unsafeReadLayerByteOff  #-}
unsafeWriteLayerByteOff !off !t = pokeLayerByteOff @layer @t @cfg off (coerce t) ; {-# INLINE unsafeWriteLayerByteOff #-}

readLayer  :: forall layer t cfg m. (KnownLayer t layer, Layer t layer cfg, MonadIO m) => Component t cfg -> m (LayerData t layer cfg)
writeLayer :: forall layer t cfg m. (KnownLayer t layer, Layer t layer cfg, MonadIO m) => Component t cfg ->   (LayerData t layer cfg) -> m ()
readLayer  = unsafeReadLayerByteOff  @layer (layerOffset @t @layer) ; {-# INLINE readLayer  #-}
writeLayer = unsafeWriteLayerByteOff @layer (layerOffset @t @layer) ; {-# INLINE writeLayer #-}






-------------------------------------
-- === Global Layer Management === --
-------------------------------------

-- === Layer registry === --

type family AllLayers component :: [Type]


-- === LayerOffset === --

type LayerOffset component layer = LayerOffset' 0 component layer (AllLayers component)
type family LayerOffset' off component layer ls where
    LayerOffset' s _ l (l ': _)  = s
    LayerOffset' s t l (p ': ps) = LayerOffset' (s + LayerSize t p) t l ps

type KnownLayer component layer = KnownType (LayerOffset component layer)
layerOffset :: forall component layer. (KnownLayer component layer) => Int
layerOffset = fromInteger $ fromType @(LayerOffset component layer) ; {-# INLINE layerOffset #-}







type instance LayerSize TERM Model = 3 * SizeOf Int
instance Layer     TERM Model (FormatTag f) where
    type LayerData TERM Model (FormatTag f) = TermModel (FormatTag f)

instance Storable (LayerData TERM Model (TermTag f))
      => Layer     TERM Model (TermTag f) where
    type LayerData TERM Model (TermTag f) = TermModel (TermTag f)
    peekLayerIO ptr = peek (ptr `plusPtr` constructorSize) ; {-# INLINE peekLayerIO #-}


instance Layer     LINK Model a where
    type LayerData LINK Model a = LinkData Draft Draft






type instance AllLayers TERM = '[Model]
type instance AllLayers LINK = '[Model]




-- type instance LayerData Model (Tag t a) = TermModel (Tag t a)

-- class LayerStorable layer (layout :: Layout) where
--     type family LayerData layer layout :: *
--     layerByteSize   :: Int
--     layerByteOffset :: Int
--
-- instance LayerStorable Model

constructorSize :: Int
constructorSize = sizeOf' @Int ; {-# INLINE constructorSize #-}

chunkSize :: Int
chunkSize = sizeOf' @Int ; {-# INLINE chunkSize #-}

-- instance Storable (TermUni fmt a) where
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
mockNewIR = Component . coerce <$> MemPool.alloc @(TermModel Draft) ; {-# INLINE mockNewIR #-}

mockNewLink :: forall m. MonadIO m => m (Link (Draft :-: Draft))
mockNewLink = Component . coerce <$> MemPool.alloc @(LinkData Draft Draft)






layerLoc0 :: Int
layerLoc0 = 0 ; {-# INLINE layerLoc0 #-}

test :: IO ()
test = do
    var1 <- mockNewIR
    var2 <- mockNewIR
    acc1 <- mockNewIR
    unsafeWriteLayerByteOff @Model layerLoc0 var1 (Var 7)
    unsafeWriteLayerByteOff @Model layerLoc0 var2 (Var 5)

    l1 <- mockNewLink
    unsafeWriteLayerByteOff @Model layerLoc0 l1 (LinkData var1 acc1)

    unsafeWriteLayerByteOff @Model layerLoc0 acc1 (Acc l1 l1)

    print var1
    x <- unsafeReadLayerByteOff @Model layerLoc0 var1
    print x




test_readWriteLayer :: Int -> IO ()
test_readWriteLayer i = do
    !ir <- mockNewIR
    unsafeWriteLayerByteOff @Model layerLoc0 ir (Var 0)
    let go 0 = return ()
        go j = do
            Var !y <- unsafeReadLayerByteOff @Model layerLoc0 ir
            Var !z <- unsafeReadLayerByteOff @Model layerLoc0 ir
            Var !x <- unsafeReadLayerByteOff @Model layerLoc0 ir
            unsafeWriteLayerByteOff @Model layerLoc0 ir (Var $! x+1)
            go $! (j - 1)
    go i
    -- Ptr.free ptr

--
-- test_readWriteLayer2 :: Int -> IO ()
-- test_readWriteLayer2 i = do
--     ir <- mockNewIR
--     writeLayer layerLoc0 ir (Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go 0 = return ()
--         go j = do
--             set <- get'
--             let layer = TypeSet.unsafeLookup @(LayerLoc Model) set
--             Var x <- readLayer layer ir
--             writeLayer layer ir (Var (x+1))
--             go (j - 1)
--     evalStateT (go i)
--         $ TypeSet.insert (XInt 1)
--         $ TypeSet.insert (6 :: Int)
--         $ TypeSet.insert layerLoc0
--         $ mempty



test_readWriteLayer_ptrOff :: Int -> IO ()
test_readWriteLayer_ptrOff i = do
    -- ptr <- mallocBytes (sizeOf' @Int * _MAX_LAYERS)
    ptr <- Ptr.new (0 :: Int)
    ir <- mockNewIR
    unsafeWriteLayerByteOff @Model layerLoc0 ir (Var 0)
    let -- go :: Int -> StateT Int IO ()
        go 0 = return ()
        go j = do
            -- set <- get'
            layer <- peek ptr
            Var x <- unsafeReadLayerByteOff @Model layer ir
            unsafeWriteLayerByteOff @Model layer ir (Var (x+1))
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
    unsafeWriteLayerByteOff @Model layerLoc0 ir (Var 0)
    let -- go :: Int -> StateT Int IO ()
        go 0 = return ()
        go j = do
            p <- get @(Ptr Int)
            x <- get @Int
            put @Int (x+1)
            layer <- liftIO $ peek p
            Var x <- unsafeReadLayerByteOff @Model layer ir
            unsafeWriteLayerByteOff @Model layer ir (Var (x+1))
            go (j - 1)
    flip evalStateT (7::Int)
       $ flip evalStateT ptr (go i)

test_readWriteLayer_static :: Int -> IO ()
test_readWriteLayer_static i = do
    ir <- mockNewIR
    writeLayer @Model ir (Var 0)
    let -- go :: Int -> StateT Int IO ()
        go 0 = return ()
        go j = do
            Var x <- readLayer @Model ir
            writeLayer @Model ir (Var (x+1))
            go (j - 1)
    (go i)

    -- evalStateT (go i)
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



newtype XInt = XInt Int

type instance Cmp Int XInt = GT
type instance Cmp XInt Int = LT

type instance Cmp (LayerLoc Model) Int = GT
type instance Cmp Int (LayerLoc Model) = LT

-- test_readWriteLayer2 :: Int -> IO ()
-- test_readWriteLayer2 i = do
--     ir <- mockNewIR
--     writeLayer layerLoc0 ir (Var 0)
--     let -- go :: Int -> StateT Int IO ()
--         go 0 = return ()
--         go j = do
--             set <- get'
--             let layer = TypeSet.unsafeLookup @(LayerLoc Model) set
--             Var x <- readLayer layer ir
--             writeLayer layer ir (Var (x+1))
--             go (j - 1)
--     evalStateT (go i) (TypeSet.insert layerLoc0 mempty)
--     -- Ptr.free ptr


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



-- newtype TermModelDefVar (a :: Layout) = TermModelDefVar
--     { __name :: Int
--     } deriving (Show, Eq)
-- type instance TermModelDef Var a = TermModelDefVar a

-- data IRDefVar (a :: Layout) = IRDefVar
--     { __type :: {-# UNPACK #-} !(Link.Type Var a)
--     , __term :: {-# UNPACK #-} !(TermModelDefVar a)
--     }




-- data TermModelDefAcc a = TermModelDefAcc
--     { __base :: !(Link.Term Acc a)
--     , __name :: !(Link.Name Acc a)
--     } deriving (Show, Eq)
-- type instance TermModelDef Acc a = TermModelDefAcc a

-- vvv - moze nie trzeba takich datatypow jezeli trzymalibysmy to w pamieci i skakli poitnerem robiac read @LayerName ?x
-- data IRDefAcc (a :: Layout) = IRDefAcc
--     { __type :: {-# UNPACK #-} !(Link.Type Acc a)
--     , __term :: {-# UNPACK #-} !(TermModelDefAcc a)
--     }

-- data family Term t (a :: Layout)
--
-- data instance Term (TermTag t) a = Foo Int



-- type Term (l :: Layout) = TermModelDef (Base l) l


-- read @Term :: IR Draft -> IR.Term Draft
-- read @Type :: IR Draft -> IR.Link Draft -- autofollow?


-- class Reader layer where
--     readIO :: forall layout. IR layout -> IO (LayerDef SubLayout layer)

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
-- data instance TermX Acc a = TermModelDefAcc
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
