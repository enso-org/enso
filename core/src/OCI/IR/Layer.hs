{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer where

import Prologue hiding (Data, Wrapped)

import qualified Control.Lens               as Lens
import qualified Data.Construction          as Data
import qualified Foreign.Storable           as Storable
import qualified Foreign.Storable.Utils     as Storable
import qualified Foreign.Storable1          as Storable1
import qualified Foreign.Storable1          as Storable1
import qualified Foreign.Storable1.Deriving as Storable1
import qualified OCI.IR.Component           as Component
import qualified OCI.Pass.Definition        as Pass

import Foreign.Ptr            (Ptr, plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable.Utils (sizeOf')
import Foreign.Storable1      (Storable1)
import OCI.IR.Component       (Component (Component))
import OCI.Pass.Definition    (Pass)



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
type Data          layer layout = Unwrap (Cons layer (Layout layer layout))
type StorableData  layer        = Storable1 (Cons layer)


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


-- === Smart constructors === --

-- | WARNING! Using this function will result in uninitialized layer memory.
unsafeNoManager :: Manager layer
unsafeNoManager = Manager Nothing Nothing Nothing ; {-# INLINE unsafeNoManager #-}

-- | WARNING! Using this function will result in uninitialized layer memory.
unsafeOnlyDestructorManager :: Data.Destructor1 (Cons layer) IO => Manager layer
unsafeOnlyDestructorManager = Manager Nothing Nothing (Just Data.destruct1) ; {-# INLINE unsafeOnlyDestructorManager #-}

staticManager :: Default1 (Cons layer) => Manager layer
staticManager = Manager (Just def1) Nothing Nothing ; {-# INLINE staticManager #-}

dynamicManager :: ( Data.Constructor1' (Cons layer) IO
                  , Data.Destructor1   (Cons layer) IO
                  ) => Manager layer
dynamicManager = Manager Nothing (Just Data.new1) (Just Data.destruct1) ; {-# INLINE dynamicManager #-}

customStaticManager :: (∀ layout. Cons layer layout) -> Manager layer
customStaticManager !t = Manager (Just t) Nothing Nothing ; {-# INLINE customStaticManager #-}

customDynamicManager :: (∀ layout. IO (Cons layer layout))
                     -> (∀ layout. Cons layer layout -> IO ())
                     -> Manager layer
customDynamicManager !s !t = Manager Nothing (Just s) (Just t) ; {-# INLINE customDynamicManager #-}


-- === Instances === --

Lens.makeLenses ''Manager



-----------------------------------
-- === Layer Reader / Writer === --
-----------------------------------

-- === Definition === --

type Editor comp layer m =
   ( Reader comp layer m
   , Writer comp layer m
   )

class Monad m => Reader comp layer m where
    read__  :: ∀ layout. Component comp layout -> m (Data layer layout)

class Monad m => Writer comp layer m where
    write__ :: ∀ layout. Component comp layout -> Data layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m.  \
        ( StorableData layer        \
        , Wrapped (Cons layer)      \
        , MonadIO m                 \
        )

unsafePeekWrapped :: CTX => SomePtr -> m (WrappedData layer layout)
unsafePokeWrapped :: CTX => SomePtr ->   (WrappedData layer layout) -> m ()
unsafePeekWrapped !ptr = liftIO $ Storable1.peek (coerce ptr) ; {-# INLINE unsafePeekWrapped #-}
unsafePokeWrapped !ptr = liftIO . Storable1.poke (coerce ptr) ; {-# INLINE unsafePokeWrapped #-}

unsafePeek :: CTX => SomePtr -> m (Data layer layout)
unsafePeek !ptr = view (from shape) <$> unsafePeekWrapped @layer @comp @layout ptr ; {-# INLINE unsafePeek #-}
unsafePoke :: CTX => SomePtr -> (Data layer layout) -> m ()
unsafePoke !ptr d = unsafePokeWrapped @layer @comp @layout ptr $ view shape d ; {-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX => Int -> SomePtr -> m (Data layer layout)
unsafePokeByteOff :: CTX => Int -> SomePtr ->   (Data layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff :: CTX => Int -> Component comp layout
                         -> m (Data layer layout)
unsafeReadByteOff  !d = unsafePeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeReadByteOff  #-}

unsafeWriteByteOff :: CTX => Int -> Component comp layout
                          -> (Data layer layout) -> m ()
unsafeWriteByteOff !d = unsafePokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeWriteByteOff #-}

read :: ∀ layer comp layout m. Reader comp layer m
     => Component comp layout -> m (Data layer layout)
read = read__ @comp @layer @m ; {-# INLINE read #-}

write :: ∀ layer comp layout m. Writer comp layer m
      => Component comp layout -> Data layer layout -> m ()
write = write__ @comp @layer @m ; {-# INLINE write #-}

#undef CTX


-- === Instances === --

instance {-# OVERLAPPABLE #-}
         (StorableData layer, Pass.LayerByteOffsetGetter comp layer (Pass pass), Wrapped (Cons layer))
    => Reader comp layer (Pass pass) where
    read__ !comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
         (StorableData layer, Pass.LayerByteOffsetGetter comp layer (Pass pass), Wrapped (Cons layer))
    => Writer comp layer (Pass pass) where
    write__ !comp !d = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Writer comp layer m)
    => Writer comp layer (t m) where
        write__ = lift .: write__ @comp @layer ; {-# INLINE write__ #-}

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Reader comp layer m)
    => Reader comp layer (t m) where
        read__ = lift . read__ @comp @layer ; {-# INLINE read__ #-}



-- === Early resolution block === --

instance Monad m => Reader Imp  layer m     where read__ _ = impossible
instance Monad m => Reader comp Imp   m     where read__ _ = impossible
instance            Reader comp layer ImpM1 where read__ _ = impossible

instance Monad m => Writer Imp  layer m     where write__ _ _ = impossible
instance Monad m => Writer comp Imp   m     where write__ _ _ = impossible
instance            Writer comp layer ImpM1 where write__ _ _ = impossible



------------------------
-- === Layer View === --
------------------------

type ViewData     layer layout = View layer layout (Layout layer layout)
type StorableView layer layout = Storable1 (View layer layout)



----------------------------------------
-- === Layer View Reader / Writer === --
----------------------------------------

-- === Definition === --

type ViewEditor comp layer layout m =
   ( ViewReader comp layer layout m
   , ViewWriter comp layer layout m
   )

class ViewReader comp layer layout m where
    readView__ :: Component comp layout -> m (ViewData layer layout)

class ViewWriter comp layer layout m where
    writeView__ :: Component comp layout -> ViewData layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m.  \
        ( StorableView layer layout \
        , MonadIO m                 \
        )

unsafePeekView :: CTX => SomePtr -> m (ViewData layer layout)
unsafePokeView :: CTX => SomePtr ->   (ViewData layer layout) -> m ()
unsafePeekView !ptr = liftIO $ Storable1.peekByteOff (coerce ptr) consByteSize; {-# INLINE unsafePeekView #-}
unsafePokeView !ptr = liftIO . Storable1.pokeByteOff (coerce ptr) consByteSize; {-# INLINE unsafePokeView #-}

unsafePeekViewByteOff :: CTX => Int -> SomePtr -> m (ViewData layer layout)
unsafePokeViewByteOff :: CTX => Int -> SomePtr -> ViewData layer layout -> m ()
unsafePeekViewByteOff !d !ptr = unsafePeekView @layer @comp @layout
                              $ ptr `plusPtr` d
unsafePokeViewByteOff !d !ptr = unsafePokeView @layer @comp @layout
                              $ ptr `plusPtr` d
{-# INLINE unsafePeekViewByteOff #-}
{-# INLINE unsafePokeViewByteOff #-}

unsafeReadViewByteOff :: CTX => Int -> Component comp layout
                      -> m (ViewData layer layout)
unsafeReadViewByteOff !d = unsafePeekViewByteOff @layer @comp @layout d
                         . coerce
unsafeWriteViewByteOff :: CTX => Int -> Component comp layout
                       -> (ViewData layer layout) -> m ()
unsafeWriteViewByteOff !d = unsafePokeViewByteOff @layer @comp @layout d
                          . coerce
{-# INLINE unsafeReadViewByteOff  #-}
{-# INLINE unsafeWriteViewByteOff #-}

readView :: ∀ layer comp layout m. ViewReader comp layer layout m
         => Component comp layout -> m (ViewData layer layout)
readView = readView__ @comp @layer @layout @m ; {-# INLINE readView #-}

writeView :: ∀ layer comp layout m. ViewWriter comp layer layout m
          => Component comp layout -> ViewData layer layout -> m ()
writeView = writeView__ @comp @layer @layout @m ; {-# INLINE writeView #-}

#undef CTX


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( StorableView layer layout
    , Pass.LayerByteOffsetGetter comp layer (Pass pass)
    ) => ViewReader comp layer layout (Pass pass) where
    readView__ !comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadViewByteOff @layer off comp
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( StorableView layer layout
    , Pass.LayerByteOffsetGetter comp layer (Pass pass)
    ) => ViewWriter comp layer layout (Pass pass) where
    writeView__ !comp !d = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeWriteViewByteOff @layer off comp d
    {-# INLINE writeView__ #-}


-- === Early resolution block === --

instance ViewReader Imp  layer layout m     where readView__ _ = impossible
instance ViewReader comp Imp   layout m     where readView__ _ = impossible
instance ViewReader comp layer Imp    m     where readView__ _ = impossible
instance ViewReader comp layer layout ImpM1 where readView__ _ = impossible

instance ViewWriter Imp  layer layout m     where writeView__ _ _ = impossible
instance ViewWriter comp Imp   layout m     where writeView__ _ _ = impossible
instance ViewWriter comp layer Imp    m     where writeView__ _ _ = impossible
instance ViewWriter comp layer layout ImpM1 where writeView__ _ _ = impossible

