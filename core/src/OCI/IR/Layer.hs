{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer where

import Prologue hiding (Data)

import qualified Foreign.Storable       as Storable
import qualified Foreign.Storable.Utils as Storable
import qualified OCI.IR.Component       as Component
import qualified OCI.Pass.Definition    as Pass

import Foreign.Ptr            (Ptr, plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable       (Storable)
import Foreign.Storable.Utils (sizeOf')
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

type family Data  layer layout :: Type
type SomeData     layer = Data layer ()
type StorableData layer = Storable (SomeData layer)


-- === Construction === --

class IsCons1 layer t where
    cons1 :: ∀ a. t a -> Data layer a


-- === Initialization === --

class Initializer layer where
    initStatic  :: Maybe     (SomeData layer)
    initDynamic :: Maybe (IO (SomeData layer))
    initStatic  = Nothing ; {-# INLINE initStatic  #-}
    initDynamic = Nothing ; {-# INLINE initDynamic #-}
    {-# MINIMAL initStatic | initDynamic #-}


-- === General Information === --

byteSize :: ∀ layer. StorableData layer => Int
byteSize = Storable.sizeOf' @(SomeData layer) ; {-# INLINE byteSize #-}



-----------------------------------
-- === Layer Reader / Writer === --
-----------------------------------

-- === Definition === --

type Editor comp layer m =
   ( Reader comp layer m
   , Writer comp layer m
   )

class Reader comp layer m where
    read__  :: ∀ layout. Component comp layout -> m (Data layer layout)

class Writer comp layer m where
    write__ :: ∀ layout. Component comp layout -> Data layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m. (StorableData layer, MonadIO m)

unsafePeek :: CTX => SomePtr -> m (SomeData layer)
unsafePoke :: CTX => SomePtr ->   (SomeData layer) -> m ()
unsafePeek !ptr = liftIO $ Storable.peek (coerce ptr) ; {-# INLINE unsafePeek #-}
unsafePoke !ptr = liftIO . Storable.poke (coerce ptr) ; {-# INLINE unsafePoke #-}

unsafePeekGen :: CTX => SomePtr -> m (Data layer layout)
unsafePokeGen :: CTX => SomePtr ->   (Data layer layout) -> m ()
unsafePeekGen = unsafeCoerce $ unsafePeek @layer @comp @layout @m ; {-# INLINE unsafePeekGen #-}
unsafePokeGen = unsafeCoerce $ unsafePoke @layer @comp @layout @m ; {-# INLINE unsafePokeGen #-}

unsafePeekByteOff :: CTX => Int -> SomePtr -> m (Data layer layout)
unsafePokeByteOff :: CTX => Int -> SomePtr ->   (Data layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeekGen @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePokeGen @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

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
         (StorableData layer, Pass.LayerByteOffsetGetter comp layer (Pass pass))
    => Reader comp layer (Pass pass) where
    read__ !comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
         (StorableData layer, Pass.LayerByteOffsetGetter comp layer (Pass pass))
    => Writer comp layer (Pass pass) where
    write__ !comp !d = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, Writer comp layer m)
    => Writer comp layer (t m) where
        write__ = lift .: write__ @comp @layer ; {-# INLINE write__ #-}

instance {-# OVERLAPPABLE #-} (Monad m, MonadTrans t, Reader comp layer m)
    => Reader comp layer (t m) where
        read__ = lift . read__ @comp @layer ; {-# INLINE read__ #-}



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

type family View  layer layout :: Type
type StorableView layer layout = Storable (View layer layout)



----------------------------------------
-- === Layer View Reader / Writer === --
----------------------------------------

-- === Definition === --

type ViewEditor comp layer layout m =
   ( ViewReader comp layer layout m
   , ViewWriter comp layer layout m
   )

class ViewReader comp layer layout m where
    readView__ :: Component comp layout -> m (View layer layout)

class ViewWriter comp layer layout m where
    writeView__ :: Component comp layout -> View layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m.  \
        ( StorableView layer layout \
        , MonadIO m                 \
        )

unsafePeekView :: CTX => SomePtr -> m (View layer layout)
unsafePokeView :: CTX => SomePtr ->   (View layer layout) -> m ()
unsafePeekView !ptr = liftIO $ Storable.peekByteOff (coerce ptr) consByteSize; {-# INLINE unsafePeekView #-}
unsafePokeView !ptr = liftIO . Storable.pokeByteOff (coerce ptr) consByteSize; {-# INLINE unsafePokeView #-}

unsafePeekViewByteOff :: CTX => Int -> SomePtr -> m (View layer layout)
unsafePokeViewByteOff :: CTX => Int -> SomePtr -> View layer layout -> m ()
unsafePeekViewByteOff !d !ptr = unsafePeekView @layer @comp @layout
                              $ ptr `plusPtr` d
unsafePokeViewByteOff !d !ptr = unsafePokeView @layer @comp @layout
                              $ ptr `plusPtr` d
{-# INLINE unsafePeekViewByteOff #-}
{-# INLINE unsafePokeViewByteOff #-}

unsafeReadViewByteOff :: CTX => Int -> Component comp layout
                      -> m (View layer layout)
unsafeReadViewByteOff !d = unsafePeekViewByteOff @layer @comp @layout d
                         . coerce
unsafeWriteViewByteOff :: CTX => Int -> Component comp layout
                       -> (View layer layout) -> m ()
unsafeWriteViewByteOff !d = unsafePokeViewByteOff @layer @comp @layout d
                          . coerce
{-# INLINE unsafeReadViewByteOff  #-}
{-# INLINE unsafeWriteViewByteOff #-}

readView :: ∀ layer comp layout m. ViewReader comp layer layout m
         => Component comp layout -> m (View layer layout)
readView = readView__ @comp @layer @layout @m ; {-# INLINE readView #-}

writeView :: ∀ layer comp layout m. ViewWriter comp layer layout m
          => Component comp layout -> View layer layout -> m ()
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

