{-# LANGUAGE UndecidableInstances #-}

module Luna.IR.Term.Basic where

import Prologue
import Foreign.Ptr            (Ptr, castPtr)
import Foreign.Storable       (Storable, alignment, peek, peekByteOff, poke, pokeByteOff, sizeOf)
import Foreign.Storable.Utils (sizeOf', alignment', castPtrTo, intPtr)

import qualified Data.Graph as Graph
import qualified Foreign.Memory.Pool as MemPool




import Luna.IR.Class
import OCI.IR.Term hiding (TermDef)
import qualified OCI.IR.Layout as Layout

import qualified Luna.IR.Link as Link
import Luna.IR.Format
import Data.Tag (Tag)

----------------
-- === IR === --
----------------

-- === IR Atoms === ---

type family TermDef t :: * -> *
-- data family TermDef (t :: *) (a :: Layout)


type family Term a


type Var = TermTag VAR; data VAR
newtype TermVar a = TermVar
    { __name :: Int
    } deriving (Show, Eq)
type instance TermDef Var = TermVar

type Acc = TermTag ACC; data ACC
data TermAcc a = TermAcc
    { __base :: !(Link.Term Acc a)
    , __name :: !(Link.Name Acc a)
    } deriving (Show, Eq)
type instance TermDef Acc = TermAcc

data TermUni fmt a
    = Var !Int
    | Acc !(Link.Term fmt a) !(Link.Term fmt a)
    deriving (Show, Eq)
type instance TermDef (FormatTag f) = TermUni (FormatTag f)

-- data instance TermDef



type instance Term (Tag t a) = TermDef (Tag t a) (Tag t a)


data Layer_Term

newtype LayerLoc a = LayerLoc {_byteOffset :: Int } deriving (Show)
makeLenses ''LayerLoc

termLayer :: LayerLoc Layer_Term
termLayer = LayerLoc 0 ; {-# INLINE termLayer #-}


class Storable (LayerData layer layout) => Layer layer layout where
    type family LayerData layer layout
    layerOffset :: Int
    layerOffset = 0

instance Layer Layer_Term (FormatTag f) where
    type LayerData Layer_Term (FormatTag f) = Term (FormatTag f)

-- instance Layer Layer_Term (TermTag f) where
--     type LayerData Layer_Term (TermTag f) = Term (TermTag f)
--     layerOffset = constructorSize


-- type instance LayerData Layer_Term (Tag t a) = Term (Tag t a)

-- class LayerStorable layer (layout :: Layout) where
--     type family LayerData layer layout :: *
--     layerByteSize   :: Int
--     layerByteOffset :: Int
--
-- instance LayerStorable Layer_Term

constructorSize :: Int
constructorSize = sizeOf' @Int ; {-# INLINE constructorSize #-}

chunkSize :: Int
chunkSize = sizeOf' @Int ; {-# INLINE chunkSize #-}

instance Storable (TermUni fmt a) where
    sizeOf    _ = 3 * chunkSize ; {-# INLINE sizeOf    #-}
    alignment _ = chunkSize     ; {-# INLINE alignment #-}
    peek ptr = peek (intPtr ptr) >>= \case
        0 -> Var <$> peekByteOff ptr chunkSize
        1 -> Acc <$> peekByteOff ptr chunkSize <*> peekByteOff ptr (chunkSize*2)
        _ -> error "Unrecognized constructor"
    {-# INLINE peek #-}
    poke ptr = \case
        Var !a    -> poke (intPtr ptr) 0 >> pokeByteOff ptr chunkSize a
        Acc !a !b -> poke (intPtr ptr) 1 >> pokeByteOff ptr (chunkSize*2) b
    {-# INLINE poke #-}



newtype IR (t :: *) = IR (Ptr ()) deriving (Show)
makeLenses ''IR


-- class Reader layer where

mockNewIR :: MonadIO m => m (IR Draft)
mockNewIR = IR . coerce <$> MemPool.alloc @(Term Draft) ; {-# INLINE mockNewIR #-}

readLayer :: forall layer layout m. (Layer layer layout, MonadIO m) => LayerLoc layer -> IR layout -> m (LayerData layer layout)
readLayer loc ir = liftIO $ peekByteOff (unwrap ir) (unwrap loc + layerOffset @layer @layout) ; {-# INLINE readLayer #-}

writeLayer :: forall layer layout m. (Layer layer layout, MonadIO m) => LayerLoc layer -> IR layout -> (LayerData layer layout) -> m ()
writeLayer loc ir val = liftIO $ pokeByteOff (unwrap ir) (unwrap loc + layerOffset @layer @layout) val ; {-# INLINE writeLayer #-}


test :: IO ()
test = do
    ir <- mockNewIR
    writeLayer termLayer ir (Var 7)
    x <- readLayer termLayer ir
    print ir
    print x


test_readWriteLayer :: Int -> IO ()
test_readWriteLayer i = do
    ir <- mockNewIR
    writeLayer termLayer ir (Var 0)
    let go 0 = return ()
        go j = do
            Var x <- readLayer termLayer ir
            writeLayer termLayer ir (Var (x+1))
            go (j - 1)
    go i
    -- Ptr.free ptr

-- readIO @Layer_Term

-- termLayer = Layer (sizeOf' @)
-- newtype IRDef (layers :: [Type]) = IRDef (Ptr ()) deriving (Show)


-- newtype IR a = IR (IRDef '[])
-- class Layer l where



-- var :: Int -> IR Var



-- foo = do
--     let x = undefined :: Term Var
--         y = x
--         y :: Int
--     return ()



-- newtype TermDefVar (a :: Layout) = TermDefVar
--     { __name :: Int
--     } deriving (Show, Eq)
-- type instance TermDef Var a = TermDefVar a

-- data IRDefVar (a :: Layout) = IRDefVar
--     { __type :: {-# UNPACK #-} !(Link.Type Var a)
--     , __term :: {-# UNPACK #-} !(TermDefVar a)
--     }




-- data TermDefAcc a = TermDefAcc
--     { __base :: !(Link.Term Acc a)
--     , __name :: !(Link.Name Acc a)
--     } deriving (Show, Eq)
-- type instance TermDef Acc a = TermDefAcc a

-- vvv - moze nie trzeba takich datatypow jezeli trzymalibysmy to w pamieci i skakli poitnerem robiac read @LayerName ?x
-- data IRDefAcc (a :: Layout) = IRDefAcc
--     { __type :: {-# UNPACK #-} !(Link.Type Acc a)
--     , __term :: {-# UNPACK #-} !(TermDefAcc a)
--     }

-- data family Term t (a :: Layout)
--
-- data instance Term (TermTag t) a = Foo Int



-- type Term (l :: Layout) = TermDef (Base l) l


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
-- data instance TermX Acc a = TermDefAcc
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
