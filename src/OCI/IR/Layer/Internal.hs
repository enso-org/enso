{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer.Internal where

import Prologue hiding (Data)

import qualified Foreign.Storable1   as Storable1
import qualified OCI.IR.Component    as Component
import qualified OCI.Pass.Definition as Pass

import Foreign.Ptr            (plusPtr)
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



-------------------
-- === Layer === --
-------------------

-- === Definition === --

type family Cons   comp layer        :: Type -> Type
type family Layout comp layer layout :: Type
type        Data   comp layer layout
          = Cons   comp layer (Layout comp layer layout)

type StorableCons comp layer = Storable1       (Cons comp layer)
type DefaultData  comp layer = Default1        (Cons comp layer)
type Initializer  comp layer = DataInitializer (Cons comp layer)


-- === Construction === --

class IsCons1 comp layer t where
    cons1 :: ∀ a. t a -> Cons comp layer a


-- === Initialization === --

class DataInitializer t where
    initStaticData  :: ∀ layout. Maybe     (t layout)
    initDynamicData :: ∀ layout. Maybe (IO (t layout))
    initStaticData  = Nothing ; {-# INLINE initStaticData  #-}
    initDynamicData = Nothing ; {-# INLINE initDynamicData #-}
    {-# MINIMAL initStaticData | initDynamicData #-}

instance DataInitializer (Component comp) where
    initStaticData = Just Component.unsafeNull ; {-# INLINE initStaticData #-}

initStatic :: forall comp layer layout. Initializer comp layer
           => Maybe (Cons comp layer layout)
initStatic = initStaticData @(Cons comp layer) ; {-# INLINE initStatic #-}

initDynamic :: forall comp layer layout. Initializer comp layer
           => Maybe (IO (Cons comp layer layout))
initDynamic = initDynamicData @(Cons comp layer) ; {-# INLINE initDynamic #-}


-- === General Information === --

byteSize :: ∀ comp layer. StorableCons comp layer => Int
byteSize = Storable1.sizeOf' @(Cons comp layer) ; {-# INLINE byteSize #-}



-----------------------------------
-- === Layer Reader / Writer === --
-----------------------------------

-- === Definition === --

type Editor comp layer m =
   ( Reader comp layer m
   , Writer comp layer m
   )

class Reader comp layer m where
    read__  :: ∀ layout. Component comp layout -> m (Data comp layer layout)

class Writer comp layer m where
    write__ :: ∀ layout. Component comp layout -> Data comp layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m. (StorableCons comp layer, MonadIO m)

unsafePeek :: CTX => SomePtr -> m (Data comp layer layout)
unsafePoke :: CTX => SomePtr ->   (Data comp layer layout) -> m ()
unsafePeek !ptr = liftIO $ Storable1.peek (coerce ptr) ; {-# INLINE unsafePeek #-}
unsafePoke !ptr = liftIO . Storable1.poke (coerce ptr) ; {-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX => Int -> SomePtr -> m (Data comp layer layout)
unsafePokeByteOff :: CTX => Int -> SomePtr ->   (Data comp layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff :: CTX => Int -> Component comp layout
                         -> m (Data comp layer layout)
unsafeReadByteOff  !d = unsafePeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeReadByteOff  #-}

unsafeWriteByteOff :: CTX => Int -> Component comp layout
                          -> (Data comp layer layout) -> m ()
unsafeWriteByteOff !d = unsafePokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeWriteByteOff #-}

read :: ∀ layer comp layout m. Reader comp layer m
     => Component comp layout -> m (Data comp layer layout)
read = read__ @comp @layer @m ; {-# INLINE read #-}

write :: ∀ layer comp layout m. Writer comp layer m
      => Component comp layout -> Data comp layer layout -> m ()
write = write__ @comp @layer @m ; {-# INLINE write #-}

#undef CTX


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( StorableCons comp layer
    , Pass.LayerByteOffsetGetter comp layer (Pass pass)
    ) => Reader comp layer (Pass pass) where
    read__ !comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( StorableCons comp layer
    , Pass.LayerByteOffsetGetter comp layer (Pass pass)
    ) => Writer comp layer (Pass pass) where
    write__ !comp !d = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}



-- === Early resolution block === --

instance Reader Imp  layer m     where read__ _ = impossible
instance Reader comp Imp   m     where read__ _ = impossible
instance Reader comp layer ImpM1 where read__ _ = impossible

instance Writer Imp  layer m     where write__ _ _ = impossible
instance Writer comp Imp   m     where write__ _ _ = impossible
instance Writer comp layer ImpM1 where write__ _ _ = impossible



------------------------
-- === Layer View === --
------------------------

type family ViewCons comp layer layout :: Type -> Type
type        View     comp layer layout
          = ViewCons comp layer layout (Layout comp layer layout)

type StorableViewCons comp layer layout = Storable1 (ViewCons comp layer layout)



----------------------------------------
-- === Layer View Reader / Writer === --
----------------------------------------

-- === Definition === --

type ViewEditor comp layer layout m =
   ( ViewReader comp layer layout m
   , ViewWriter comp layer layout m
   )

class ViewReader comp layer layout m where
    readView__ :: Component comp layout -> m (View comp layer layout)

class ViewWriter comp layer layout m where
    writeView__ :: Component comp layout -> View comp layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m.           \
        ( StorableViewCons comp layer layout \
        , MonadIO m                          \
        )

unsafePeekView :: CTX => SomePtr -> m (View comp layer layout)
unsafePokeView :: CTX => SomePtr ->   (View comp layer layout) -> m ()
unsafePeekView !ptr = liftIO $ Storable1.peekByteOff (coerce ptr) consByteSize; {-# INLINE unsafePeekView #-}
unsafePokeView !ptr = liftIO . Storable1.pokeByteOff (coerce ptr) consByteSize; {-# INLINE unsafePokeView #-}

unsafePeekViewByteOff :: CTX => Int -> SomePtr -> m (View comp layer layout)
unsafePokeViewByteOff :: CTX => Int -> SomePtr -> View comp layer layout -> m ()
unsafePeekViewByteOff !d !ptr = unsafePeekView @layer @comp @layout
                              $ ptr `plusPtr` d
unsafePokeViewByteOff !d !ptr = unsafePokeView @layer @comp @layout
                              $ ptr `plusPtr` d
{-# INLINE unsafePeekViewByteOff #-}
{-# INLINE unsafePokeViewByteOff #-}

unsafeReadViewByteOff :: CTX => Int -> Component comp layout
                      -> m (View comp layer layout)
unsafeReadViewByteOff !d = unsafePeekViewByteOff @layer @comp @layout d
                         . coerce
unsafeWriteViewByteOff :: CTX => Int -> Component comp layout
                       -> (View comp layer layout) -> m ()
unsafeWriteViewByteOff !d = unsafePokeViewByteOff @layer @comp @layout d
                          . coerce
{-# INLINE unsafeReadViewByteOff  #-}
{-# INLINE unsafeWriteViewByteOff #-}

readView :: ∀ layer comp layout m. ViewReader comp layer layout m
         => Component comp layout -> m (View comp layer layout)
readView = readView__ @comp @layer @layout @m ; {-# INLINE readView #-}

writeView :: ∀ layer comp layout m. ViewWriter comp layer layout m
          => Component comp layout -> View comp layer layout -> m ()
writeView = writeView__ @comp @layer @layout @m ; {-# INLINE writeView #-}

#undef CTX


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    ( StorableViewCons comp layer layout
    , Pass.LayerByteOffsetGetter comp layer (Pass pass)
    ) => ViewReader comp layer layout (Pass pass) where
    readView__ !comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadViewByteOff @layer off comp
    {-# INLINE readView__ #-}

instance {-# OVERLAPPABLE #-}
    ( StorableViewCons comp layer layout
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

