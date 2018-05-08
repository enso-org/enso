{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Layer where

import Prologue hiding (Data, Wrapped)

import qualified Control.Lens      as Lens
import qualified Data.Construction as Data
import qualified Foreign.Ptr       as Ptr
import qualified Foreign.Storable  as Storable
import qualified Foreign.Storable1 as Storable1

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

instance Default   t => Default   (Simple t layout) where def    = wrap def    ; {-# INLINE def #-}
instance Mempty    t => Mempty    (Simple t layout) where mempty = wrap mempty ; {-# INLINE mempty #-}
instance Semigroup t => Semigroup (Simple t layout) where
    a <> b = wrap (unwrap a <> unwrap b) ; {-# INLINE (<>) #-}

-- TODO[PM]: Storable1.derive ''Simple
instance Storable.Storable t => Storable1.Storable1 (Simple t) where
    sizeOf    _ = Storable.sizeOf   (undefined :: t)  ; {-# INLINE sizeOf    #-}
    alignment _ = Storable.alignment (undefined :: t) ; {-# INLINE alignment #-}
    peek      p = wrap <$> Storable.peek (coerce p)   ; {-# INLINE peek      #-}
    poke      p = Storable.poke (coerce p) . unwrap   ; {-# INLINE poke      #-}



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


-- === General Information === --

byteSize :: ∀ layer. StorableData layer => Int
byteSize = Storable1.sizeOf' @(Cons layer) ; {-# INLINE byteSize #-}



----------------------------------
-- === Layer Memory Manager === --
----------------------------------

-- === Definition === --

data Manager (layer :: Type) = Manager
    { _initializer :: ∀ layout. Maybe (Cons layer layout)
    , _constructor :: ∀ layout. Maybe (IO (Cons layer layout))
    , _destructor  :: ∀ layout. Maybe (Cons layer layout -> IO ())
    }

data DynamicManager comp = DynamicManager
    { _dynamicInitializer :: SomePtr
    , _dynamicConstructor :: SomePtr -> IO ()
    , _dynamicDestructor  :: SomePtr -> IO ()
    }


-- === Smart constructors === --

-- | WARNING! Using this function will result in uninitialized layer memory.
unsafeNoManager :: Manager layer
unsafeNoManager = Manager Nothing Nothing Nothing ; {-# INLINE unsafeNoManager #-}

-- | WARNING! Using this function will result in uninitialized layer memory.
unsafeOnlyDestructorManager :: ∀ layer. Data.ShallowDestructor1 IO (Cons layer)
                            => Manager layer
unsafeOnlyDestructorManager = Manager Nothing Nothing
                            $ Just Data.destructShallow1
{-# INLINE unsafeOnlyDestructorManager #-}

staticManager :: Default1 (Cons layer) => Manager layer
staticManager = Manager (Just def1) Nothing Nothing ; {-# INLINE staticManager #-}

dynamicManager :: ( Data.Constructor1 IO () (Cons layer)
                  , Data.ShallowDestructor1 IO (Cons layer)
                  ) => Manager layer
dynamicManager = Manager Nothing
                 (Just Data.construct1')
                 (Just Data.destructShallow1)
{-# INLINE dynamicManager #-}

customStaticManager :: (∀ layout. Cons layer layout) -> Manager layer
customStaticManager !t = Manager (Just t) Nothing Nothing ; {-# INLINE customStaticManager #-}

customDynamicManager :: (∀ layout. IO (Cons layer layout))
                     -> (∀ layout. Cons layer layout -> IO ())
                     -> Manager layer
customDynamicManager !s !t = Manager Nothing (Just s) (Just t) ; {-# INLINE customDynamicManager #-}


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

class Monad m => Reader t layer m where
    read__  :: ∀ layout. t layout -> m (Data layer layout)

class Monad m => Writer t layer m where
    write__ :: ∀ layout. t layout -> Data layer layout -> m ()


-- === API === --

type StorableLayer layer m =
    ( StorableData layer
    , Wrapped (Cons layer)
    , MonadIO m
    )

#define CTX1 ∀ layer   layout m.   StorableLayer layer m
#define CTX2 ∀ layer t layout m. ( StorableLayer layer m           \
                                 , Convertible' (t layout) SomePtr \
                                 )

unsafePeekWrapped :: CTX1 => SomePtr -> m (WrappedData layer layout)
unsafePokeWrapped :: CTX1 => SomePtr ->   (WrappedData layer layout) -> m ()
unsafePeekWrapped !ptr = liftIO $ Storable1.peek (coerce ptr) ; {-# INLINE unsafePeekWrapped #-}
unsafePokeWrapped !ptr = liftIO . Storable1.poke (coerce ptr) ; {-# INLINE unsafePokeWrapped #-}

unsafePeek :: CTX1 => SomePtr -> m (Data layer layout)
unsafePeek !p = view (from shape) <$> unsafePeekWrapped @layer @layout p ; {-# INLINE unsafePeek #-}
unsafePoke :: CTX1 => SomePtr -> (Data layer layout) -> m ()
unsafePoke !p d = unsafePokeWrapped @layer @layout p $ view shape d ; {-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX1 => Int -> SomePtr -> m (Data layer layout)
unsafePokeByteOff :: CTX1 => Int -> SomePtr ->   (Data layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeek @layer @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePoke @layer @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff  :: CTX2 => Int -> t layout -> m (Data layer layout)
unsafeWriteByteOff :: CTX2 => Int -> t layout -> (Data layer layout) -> m ()
unsafeReadByteOff  !d = unsafePeekByteOff @layer @layout d . convert' ; {-# INLINE unsafeReadByteOff  #-}
unsafeWriteByteOff !d = unsafePokeByteOff @layer @layout d . convert' ; {-# INLINE unsafeWriteByteOff #-}

read  :: ∀ layer t lyt m. Reader t layer m => t lyt -> m (Data layer lyt)
write :: ∀ layer t lyt m. Writer t layer m => t lyt -> Data layer lyt -> m ()
read  = read__  @t @layer @m ; {-# INLINE read  #-}
write = write__ @t @layer @m ; {-# INLINE write #-}

#undef CTX1
#undef CTX2


-- === Instances === --

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Writer comp layer m)
    => Writer comp layer (t m) where
        write__ = lift .: write__ @comp @layer ; {-# INLINE write__ #-}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Reader comp layer m)
    => Reader comp layer (t m) where
        read__ = lift . read__ @comp @layer ; {-# INLINE read__ #-}



-- === Early resolution block === --

instance Monad m => Reader ImpM1 layer m     where read__ _ = impossible
instance Monad m => Reader comp  Imp   m     where read__ _ = impossible
instance            Reader comp  layer ImpM1 where read__ _ = impossible

instance Monad m => Writer ImpM1 layer m     where write__ _ _ = impossible
instance Monad m => Writer comp  Imp   m     where write__ _ _ = impossible
instance            Writer comp  layer ImpM1 where write__ _ _ = impossible



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


-- === API === --

type StorableLayerView layer layout m =
    ( StorableView layer layout
    , MonadIO m
    )

#define CTX1 ∀ layer   layout m.   StorableLayerView layer layout m
#define CTX2 ∀ layer t layout m. ( StorableLayerView layer layout m \
                                 , Convertible' (t layout) SomePtr  \
                                 )

unsafePeekView :: CTX1 => SomePtr -> m (ViewData layer layout)
unsafePokeView :: CTX1 => SomePtr ->   (ViewData layer layout) -> m ()
unsafePeekView !ptr = liftIO $ Storable1.peekByteOff (coerce ptr) consByteSize; {-# INLINE unsafePeekView #-}
unsafePokeView !ptr = liftIO . Storable1.pokeByteOff (coerce ptr) consByteSize; {-# INLINE unsafePokeView #-}

unsafePeekViewByteOff :: CTX1 => Int -> SomePtr -> m (ViewData layer layout)
unsafePokeViewByteOff :: CTX1 => Int -> SomePtr -> ViewData layer layout -> m ()
unsafePeekViewByteOff !d !ptr = unsafePeekView @layer @layout $ ptr `plusPtr` d
unsafePokeViewByteOff !d !ptr = unsafePokeView @layer @layout $ ptr `plusPtr` d
{-# INLINE unsafePeekViewByteOff #-}
{-# INLINE unsafePokeViewByteOff #-}

unsafeReadViewByteOff :: CTX2 => Int -> t layout
                      -> m (ViewData layer layout)
unsafeReadViewByteOff !d = unsafePeekViewByteOff @layer @layout d . convert'
unsafeWriteViewByteOff :: CTX2 => Int -> t layout
                       -> (ViewData layer layout) -> m ()
unsafeWriteViewByteOff !d = unsafePokeViewByteOff @layer @layout d . convert'
{-# INLINE unsafeReadViewByteOff  #-}
{-# INLINE unsafeWriteViewByteOff #-}

readView :: ∀ layer t layout m. ViewReader t layer layout m
         => t layout -> m (ViewData layer layout)
readView = readView__ @t @layer @layout @m ; {-# INLINE readView #-}

writeView :: ∀ layer t layout m. ViewWriter t layer layout m
          => t layout -> ViewData layer layout -> m ()
writeView = writeView__ @t @layer @layout @m ; {-# INLINE writeView #-}

#undef CTX1
#undef CTX2


-- === Early resolution block === --

instance ViewReader ImpM1 layer layout m     where readView__ _ = impossible
instance ViewReader comp  Imp   layout m     where readView__ _ = impossible
instance ViewReader comp  layer Imp    m     where readView__ _ = impossible
instance ViewReader comp  layer layout ImpM1 where readView__ _ = impossible

instance ViewWriter ImpM1 layer layout m     where writeView__ _ _ = impossible
instance ViewWriter comp  Imp   layout m     where writeView__ _ _ = impossible
instance ViewWriter comp  layer Imp    m     where writeView__ _ _ = impossible
instance ViewWriter comp  layer layout ImpM1 where writeView__ _ _ = impossible



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
