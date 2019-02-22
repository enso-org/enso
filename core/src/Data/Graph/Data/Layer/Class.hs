{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Layer.Class where

import Prologue hiding (Data, Wrapped, convert, convert1, read)

import qualified Control.Lens                as Lens
import qualified Data.Construction           as Data
import qualified Data.Convert2               as Convert
import qualified Foreign.Storable            as Storable
import qualified Foreign.Storable1           as Storable1

import Data.Convert2          (convert1)
import Foreign.Ptr            (plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable.Utils (sizeOf')
import Foreign.Storable1      (Storable1)



-----------------------
-- === Constants === --
-----------------------

consByteSize :: Int
consByteSize = sizeOf' @Int ; {-# INLINE consByteSize #-}



-------------------------
-- === Layer Shape === --
-------------------------

-- === Definition === --

newtype Simple t layout = Simple t
makeLenses ''Simple


-- === API === --

type IsWrapped   t = (CheckSimple t ~ 'True)
type IsUnwrapped t = (CheckSimple t ~ 'False)
type family CheckSimple t where
    CheckSimple (Simple _) = 'True
    CheckSimple _          = 'False

type family Unwrap t where
    Unwrap (t a) = Unwrap__ (CheckSimple t) (t a)

type family Unwrap__ isSimple t where
    Unwrap__ 'True (Simple t _) = t
    Unwrap__ 'False t           = t

class Wrapped t where shape :: ∀ a. Iso' (Unwrap (t a)) (t a)
instance {-# OVERLAPPABLE #-} IsUnwrapped t
      => Wrapped t          where shape = id      ; {-# INLINE shape #-}
instance Wrapped (Simple t) where shape = coerced ; {-# INLINE shape #-}


-- === Intances === --

instance Show t => Show (Simple t layout) where
    show = show . unwrap ; {-# INLINE show #-}


instance Default t => Default1 (Simple t) where
    def1 = wrap def ; {-# INLINE def1 #-}

instance Data.ShallowDestructor m t => Data.ShallowDestructor1 m (Simple t) where
    destructShallow1 = Data.destructShallow . unwrap ; {-# INLINE destructShallow1 #-}

instance Default   t => Default   (Simple t layout) where def    = wrap def    ; {-# INLINE def #-}
instance Mempty    t => Mempty    (Simple t layout) where mempty = wrap mempty ; {-# INLINE mempty #-}
instance Semigroup t => Semigroup (Simple t layout) where
    a <> b = wrap (unwrap a <> unwrap b) ; {-# INLINE (<>) #-}

-- TODO[Ara]: Storable1.derive ''Simple
instance Storable.Storable t => Storable1.Storable1 (Simple t) where
    sizeOf    = \ ~_ -> Storable.sizeOf    (undefined :: t) ; {-# INLINE sizeOf    #-}
    alignment = \ ~_ -> Storable.alignment (undefined :: t) ; {-# INLINE alignment #-}
    peek      = \p   -> wrap <$> Storable.peek (coerce p)   ; {-# INLINE peek      #-}
    poke      = \p   -> Storable.poke (coerce p) . unwrap   ; {-# INLINE poke      #-}

instance Storable.Storable t => Storable.Storable (Simple t layout) where
    sizeOf    = \ ~_ -> Storable.sizeOf    (undefined :: t) ; {-# INLINE sizeOf    #-}
    alignment = \ ~_ -> Storable.alignment (undefined :: t) ; {-# INLINE alignment #-}
    peek      = \p   -> wrap <$> Storable.peek (coerce p)   ; {-# INLINE peek      #-}
    poke      = \p   -> Storable.poke (coerce p) . unwrap   ; {-# INLINE poke      #-}



------------------------------
-- === Layer ByteOffset === --
------------------------------

-- === Definition === --

class KnownLayer (t :: Type -> Type) layer (m :: Type -> Type) where
    layerByteOffset :: Int


-- === Instances === --

instance {-# OVERLAPPABLE #-} KnownLayer t layer m
      => KnownLayer t layer (f m) where
    layerByteOffset = layerByteOffset @t @layer @m
    {-# INLINE layerByteOffset #-}



-------------------
-- === Layer === --
-------------------

-- === Definition === --

class Layer layer where
    type family Layout layer layout :: Type
    type family Cons   layer        :: Type -> Type
    type family View   layer layout :: Type -> Type
    manager :: Manager layer

    type Layout layer layout = layout
    type View   layer layout = Cons layer
    default manager :: Default1 (Cons layer) => Manager layer
    manager = staticManager ; {-# INLINE manager #-}

type WrappedData   layer layout = Cons layer (Layout layer layout)
type Data          layer layout = Unwrap     (Cons layer (Layout layer layout))
type ViewData      layer layout = View       layer layout (Layout layer layout)
type StorableData  layer        = Storable1  (Cons layer)
type StorableView  layer layout = Storable1  (View layer layout)


-- === Construction === --

class IsCons1 layer t where
    cons1 :: ∀ a. t a -> Data layer a


-- === Byte Size === --

class KnownByteSize t where
    byteSize :: Int

instance StorableData layer
      => KnownByteSize (layer :: Type) where
    byteSize = Storable1.sizeOf' @(Cons layer)
    {-# INLINE byteSize #-}

instance KnownByteSize '[] where
    byteSize = 0
    {-# INLINE byteSize #-}

instance (KnownByteSize layer, KnownByteSize layers)
        => KnownByteSize (layer ': layers) where
    byteSize = byteSize @layer + byteSize @layers
    {-# INLINE byteSize #-}



----------------------------------
-- === Layer Memory Manager === --
----------------------------------

-- === Definition === --

data Manager (layer :: Type) = Manager
    { _initializer :: ∀ layout. Maybe (IO (Cons layer layout))
    , _destructor  :: ∀ layout. Maybe (Cons layer layout -> IO ())
    }

data DynamicManager comp = DynamicManager
    { _dynamicInitializer :: !SomePtr
    , _dynamicDestructor  :: !(SomePtr -> IO ())
    }


-- === Smart constructors === --

-- | WARNING! Using this function will result in uninitialized layer memory.
unsafeNoManager :: Manager layer
unsafeNoManager = Manager Nothing Nothing ; {-# INLINE unsafeNoManager #-}

-- | WARNING! Using this function will result in uninitialized layer memory.
unsafeOnlyDestructorManager :: ∀ layer. Data.ShallowDestructor1 IO (Cons layer)
                            => Manager layer
unsafeOnlyDestructorManager = Manager Nothing
                            $ Just Data.destructShallow1
{-# INLINE unsafeOnlyDestructorManager #-}

staticManager :: Default1 (Cons layer) => Manager layer
staticManager = Manager (Just $ pure def1) Nothing ; {-# INLINE staticManager #-}

dynamicManager :: ( Default1 (Cons layer)
                  , Data.ShallowDestructor1 IO (Cons layer)
                  ) => Manager layer
dynamicManager = Manager (Just $ pure def1)
                         (Just Data.destructShallow1)
{-# INLINE dynamicManager #-}

customStaticManager :: (∀ layout. IO (Cons layer layout)) -> Manager layer
customStaticManager !t = Manager (Just t) Nothing ; {-# INLINE customStaticManager #-}

-- customDynamicManager :: (∀ layout. IO (Cons layer layout))
--                      -> (∀ layout. Cons layer layout -> IO ())
--                      -> Manager layer
-- customDynamicManager !s !t = Manager Nothing (Just s) (Just t) ; {-# INLINE customDynamicManager #-}


-- === Instances === --

Lens.makeLenses ''Manager
Lens.makeLenses ''DynamicManager



-----------------------------------
-- === Layer Reader / Writer === --
-----------------------------------

-- === Definition === --

type Editor t layer m =
   ( Reader t layer m
   , Writer t layer m
   )

type EditorCtx t layer m = (MonadIO m, KnownLayer t layer m)

class EditorCtx t layer m
   => Reader t layer m where
    read__ :: ∀ layout. t layout -> m (Data layer layout)

class EditorCtx t layer m
   => Writer t layer m where
    write__ :: ∀ layout. t layout -> Data layer layout -> m ()


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( StorableLayerX layer
    , Convert.To1 SomePtr t
    , KnownLayer t layer m
    , MonadIO m
    ) => Reader t layer m where
    read__ = unsafeReadByteOff @layer (layerByteOffset @t @layer @m)
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( StorableLayerX layer
    , Convert.To1 SomePtr t
    , KnownLayer t layer m
    , MonadIO m
    ) => Writer t layer m where
    write__ = unsafeWriteByteOff @layer (layerByteOffset @t @layer @m)
    {-# INLINE write__ #-}

instance {-# OVERLAPPABLE #-} (MonadIO (f m), MonadTrans f, Reader t layer m)
    => Reader t layer (f m) where
        read__ = \t -> lift $ read__ @t @layer t
        {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-} (MonadIO (f m), MonadTrans f, Writer t layer m)
    => Writer t layer (f m) where
        write__ = \t a -> lift $ write__ @t @layer t a
        {-# INLINE write__ #-}


-- === Pre-mature resolution block === --

instance EditorCtx t Imp m => Reader t Imp m where read__  = impossible
instance EditorCtx t Imp m => Writer t Imp m where write__ = impossible


-- === API === --

type StorableLayer layer m =
    ( StorableData layer
    , Wrapped (Cons layer)
    , MonadIO m
    )

type StorableLayerX layer =
    ( StorableData layer
    , Wrapped (Cons layer)
    )

#define CTX1 ∀ layer   layout m.   StorableLayer layer m
#define CTX2 ∀ layer t layout m. ( StorableLayer layer m \
                                 , Convert.To1 SomePtr t \
                                 )

unsafePeekWrapped :: CTX1 => SomePtr -> m (WrappedData layer layout)
unsafePokeWrapped :: CTX1 => SomePtr ->   (WrappedData layer layout) -> m ()
unsafePeekWrapped = \ptr -> liftIO $ Storable1.peek (coerce ptr)
unsafePokeWrapped = \ptr -> liftIO . Storable1.poke (coerce ptr)
{-# INLINE unsafePeekWrapped #-}
{-# INLINE unsafePokeWrapped #-}

unsafePeek :: CTX1 => SomePtr -> m (Data layer layout)
unsafePoke :: CTX1 => SomePtr -> (Data layer layout) -> m ()
unsafePeek = \p   -> view (from shape) <$> unsafePeekWrapped @layer @layout p
unsafePoke = \p d -> unsafePokeWrapped @layer @layout p $ view shape d
{-# INLINE unsafePeek #-}
{-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX1 => Int -> SomePtr -> m (Data layer layout)
unsafePokeByteOff :: CTX1 => Int -> SomePtr ->   (Data layer layout) -> m ()
unsafePeekByteOff = \d ptr -> unsafePeek @layer @layout (ptr `plusPtr` d)
unsafePokeByteOff = \d ptr -> unsafePoke @layer @layout (ptr `plusPtr` d)
{-# INLINE unsafePeekByteOff #-}
{-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff  :: CTX2 => Int -> t layout -> m (Data layer layout)
unsafeWriteByteOff :: CTX2 => Int -> t layout -> (Data layer layout) -> m ()
unsafeReadByteOff  = \d -> unsafePeekByteOff @layer @layout d . convert1
unsafeWriteByteOff = \d -> unsafePokeByteOff @layer @layout d . convert1
{-# INLINE unsafeReadByteOff  #-}
{-# INLINE unsafeWriteByteOff #-}

read  :: ∀ layer t lyt m. Reader t layer m => t lyt -> m (Data layer lyt)
write :: ∀ layer t lyt m. Writer t layer m => t lyt -> Data layer lyt -> m ()
read  = read__  @t @layer
write = write__ @t @layer
{-# INLINE read  #-}
{-# INLINE write #-}

modify  :: ∀ layer t lyt m a. (Reader t layer m, Writer t layer m)
        => t lyt -> (Data layer lyt -> (a, Data layer lyt)) -> m a
modify_ :: ∀ layer t lyt m . (Reader t layer m, Writer t layer m)
        => t lyt -> (Data layer lyt ->     Data layer lyt)  -> m ()
modify comp f = do
    old <- read @layer comp
    let (!res, !new) = f old
    write @layer comp new
    pure res
modify_ comp f = read @layer comp >>= write @layer comp . f
{-# INLINE modify  #-}
{-# INLINE modify_ #-}

#undef CTX1
#undef CTX2



----------------------------------------
-- === Layer View Reader / Writer === --
----------------------------------------

-- === Definition === --

type ViewEditor comp layer layout m =
   ( ViewReader comp layer layout m
   , ViewWriter comp layer layout m
   )

class ViewReader t layer layout m where
    readView__ :: t layout -> m (ViewData layer layout)

class ViewWriter t layer layout m where
    writeView__ :: t layout -> ViewData layer layout -> m ()


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( StorableView layer layout
    , Reader t layer m
    , Convert.To1 SomePtr t
    ) => ViewReader t layer layout m where
    readView__ = unsafeReadViewByteOff @layer (layerByteOffset @t @layer @m)
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( StorableView layer layout
    , Writer t layer m
    , Convert.To1 SomePtr t
    ) => ViewWriter t layer layout m where
    writeView__ = unsafeWriteViewByteOff @layer (layerByteOffset @t @layer @m)
    {-# INLINE writeView__ #-}


-- === Pre-mature resolution block === --

instance ViewReader t Imp   layout m where readView__  = impossible
instance ViewWriter t Imp   layout m where writeView__ = impossible
instance ViewReader t layer Imp    m where readView__  = impossible
instance ViewWriter t layer Imp    m where writeView__ = impossible


-- === API === --

type StorableLayerView layer layout m =
    ( StorableView layer layout
    , MonadIO m
    )

#define CTX1 ∀ layer   lyt m.   StorableLayerView layer lyt m
#define CTX2 ∀ layer t lyt m. ( StorableLayerView layer lyt m \
                              , Convert.To1 SomePtr t         \
                              )

unsafePeekView :: CTX1 => SomePtr -> m (ViewData layer lyt)
unsafePokeView :: CTX1 => SomePtr ->   (ViewData layer lyt) -> m ()
unsafePeekView = \p -> liftIO $ Storable1.peekByteOff (coerce p) consByteSize
unsafePokeView = \p -> liftIO . Storable1.pokeByteOff (coerce p) consByteSize
{-# INLINE unsafePeekView #-}
{-# INLINE unsafePokeView #-}

unsafePeekViewByteOff :: CTX1 => Int -> SomePtr -> m (ViewData layer lyt)
unsafePokeViewByteOff :: CTX1 => Int -> SomePtr -> ViewData layer lyt -> m ()
unsafePeekViewByteOff !d !ptr = unsafePeekView @layer @lyt $ ptr `plusPtr` d
unsafePokeViewByteOff !d !ptr = unsafePokeView @layer @lyt $ ptr `plusPtr` d
{-# INLINE unsafePeekViewByteOff #-}
{-# INLINE unsafePokeViewByteOff #-}

unsafeReadViewByteOff  :: CTX2 => Int -> t lyt -> m (ViewData layer lyt)
unsafeWriteViewByteOff :: CTX2 => Int -> t lyt -> (ViewData layer lyt) -> m ()
unsafeReadViewByteOff  = \d -> unsafePeekViewByteOff @layer @lyt d . convert1
unsafeWriteViewByteOff = \d -> unsafePokeViewByteOff @layer @lyt d . convert1
{-# INLINE unsafeReadViewByteOff  #-}
{-# INLINE unsafeWriteViewByteOff #-}

readView :: ∀ layer t lyt m. ViewReader t layer lyt m
         => t lyt -> m (ViewData layer lyt)
readView = readView__ @t @layer @lyt @m
{-# INLINE readView #-}

writeView :: ∀ layer t lyt m. ViewWriter t layer lyt m
          => t lyt -> ViewData layer lyt -> m ()
writeView = writeView__ @t @layer @lyt @m
{-# INLINE writeView #-}

#undef CTX1
#undef CTX2


-----------------
-- === Rep === --
-----------------

-- === Definition === --

newtype Rep = Rep SomeTypeRep deriving (Eq, Ord, Show)
makeLenses ''Rep


-- === API === --

rep  :: ∀ layer.  Typeable layer        => Rep
reps :: ∀ layers. (TypeableMany layers) => [Rep]
rep  = wrap  $  someTypeRep  @layer  ; {-# INLINE rep  #-}
reps = wrap <$> someTypeReps @layers ; {-# INLINE reps #-}
