{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer.Internal where

import Prologue hiding (Data)

import qualified Foreign.Storable1 as Storable1

import Foreign.Ptr            (plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable.Utils (sizeOf')
import Foreign.Storable1      (Storable1)
import OCI.IR.Component       (Component (Component))


-----------------------
-- === Constants === --
-----------------------

constructorSize :: Int
constructorSize = sizeOf' @Int ; {-# INLINE constructorSize #-}



-------------------
-- === Layer === --
-------------------


-- === Definition === --

type Cons = Type -> Type

type family Data       comp layer        :: Cons
type family Layout     comp layer layout :: Type
type family ConsData   comp layer layout :: Cons
type family ConsLayout (cons :: Cons)    :: Type

type StorableData     comp layer        = Storable1 (Data comp layer)
-- type StorableConsData comp layer layout = Storable1 (ConsData comp layer
--                                                         (Layout.GetBase layout))

type DefaultData comp layer = Default1 (Data comp layer)

class Layer comp layer where
    init :: ∀ layout. Data comp layer layout

-- instance {-# OVERLAPPABLE #-} Layer comp layer where
--     init = Nothing ; {-# INLINE init #-}

-- class Layer comp layer where
--     init :: ∀ layout. Ptr (Data comp layer layout) -> IO ()
--
-- instance {-# OVERLAPPABLE #-}
--     (StorableData comp layer, Default1 (Data comp layer))
--     => Layer comp layer where
--     init !ptr = Storable1.poke ptr def1 ; {-# INLINE init #-}


type Data' comp layer layout = Data comp layer (Layout comp layer layout)

-- === General Information === --

byteSize :: ∀ comp layer. StorableData comp layer => Int
byteSize = Storable1.sizeOf' @(Data comp layer) ; {-# INLINE byteSize #-}



-----------------------------------
-- === Layer Reader / Writer === --
-----------------------------------

-- === Definition === --

type Editor comp layer m =
   ( Reader comp layer m
   , Writer comp layer m
   )

class Reader comp layer m where
    read__  :: ∀ layout. Component comp layout -> m (Data' comp layer layout)

class Writer comp layer m where
    write__ :: ∀ layout. Component comp layout -> Data' comp layer layout -> m ()


-- === API === --

#define CTX ∀ layer comp layout m. (StorableData comp layer, MonadIO m)

unsafePeek :: CTX => SomePtr -> m (Data' comp layer layout)
unsafePoke :: CTX => SomePtr ->   (Data' comp layer layout) -> m ()
unsafePeek !ptr = liftIO $ Storable1.peek (coerce ptr) ; {-# INLINE unsafePeek #-}
unsafePoke !ptr = liftIO . Storable1.poke (coerce ptr) ; {-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX => Int -> SomePtr -> m (Data' comp layer layout)
unsafePokeByteOff :: CTX => Int -> SomePtr ->   (Data' comp layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff ::
    CTX => Int -> Component comp layout -> m (Data' comp layer layout)
unsafeReadByteOff  !d = unsafePeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeReadByteOff  #-}

unsafeWriteByteOff ::
    CTX => Int -> Component comp layout -> (Data' comp layer layout) -> m ()
unsafeWriteByteOff !d = unsafePokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeWriteByteOff #-}

read :: ∀ layer comp layout m. Reader comp layer m
     => Component comp layout -> m (Data' comp layer layout)
read = read__ @comp @layer @m ; {-# INLINE read #-}

write :: ∀ layer comp layout m. Writer comp layer m
      => Component comp layout -> Data' comp layer layout -> m ()
write = write__ @comp @layer @m ; {-# INLINE write #-}

#undef CTX


-- === Instances === --

-- instance {-# OVERLAPPABLE #-}
--     (StorableData comp layer, MonadIO m, Pass.LayerByteOffsetGetter comp layer m)
--     => Reader comp layer m where
--     read__ !comp = do
--         !off <- Pass.getLayerByteOffset @comp @layer
--         unsafeReadByteOff @layer off comp
--     {-# INLINE read__ #-}
--
-- instance {-# OVERLAPPABLE #-}
--     (StorableData comp layer, MonadIO m, Pass.LayerByteOffsetGetter comp layer m)
--     => Writer comp layer m where
--     write__ !comp !d = do
--         !off <- Pass.getLayerByteOffset @comp @layer
--         unsafeWriteByteOff @layer off comp d
--     {-# INLINE write__ #-}


-- === Early resolution block === --

instance Reader Imp  layer m    where read__ _ = impossible
instance Reader comp Imp   m    where read__ _ = impossible
instance Reader comp layer ImpM1 where read__ _ = impossible

instance Writer Imp  layer m    where write__ _ _ = impossible
instance Writer comp Imp   m    where write__ _ _ = impossible
instance Writer comp layer ImpM1 where write__ _ _ = impossible



-- ----------------------------------------
-- -- === Layer Cons Reader / Writer === --
-- ----------------------------------------
--
-- -- === Ctx === --
--
-- #define CTXHeader ∀ layer comp layout out cons baseLayout m.
-- #define CTXBody                                        \
--     ( out        ~ cons layout                         \
--     , cons       ~ ConsData comp layer baseLayout      \
--     , baseLayout ~ ConsLayout cons                     \
--     , baseLayout ~ Layout.GetBase layout               \
--     , StorableConsData comp layer layout               \
--     , MonadIO m                                        \
--     )
-- #define CTX CTXHeader CTXBody
--
--
-- -- === Definition === --
--
-- type ConsEditor comp layer m =
--    ( ConsReader comp layer m
--    , ConsWriter comp layer m
--    )
--
-- class ConsReader comp layer m where
--     readCons__ :: ∀ layout baseLayout cons out. CTXBody
--                => Component comp layout -> m out
--
-- class ConsWriter comp layer m where
--     writeCons__ :: ∀ layout baseLayout cons out. CTXBody
--                 => Component comp layout -> out -> m ()
--
--
-- -- === API === --
--
-- unsafeConsPeek :: CTX => SomePtr -> m out
-- unsafeConsPoke :: CTX => SomePtr ->   out -> m ()
-- unsafeConsPeek !ptr = liftIO $ Storable1.peekByteOff ptr constructorSize ; {-# INLINE unsafeConsPeek #-}
-- unsafeConsPoke !ptr = liftIO . Storable1.pokeByteOff ptr constructorSize ; {-# INLINE unsafeConsPoke #-}
--
-- unsafeConsPeekByteOff :: CTX => Int -> SomePtr -> m out
-- unsafeConsPokeByteOff :: CTX => Int -> SomePtr ->   out -> m ()
-- unsafeConsPokeByteOff !d !ptr = unsafeConsPoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafeConsPokeByteOff #-}
-- unsafeConsPeekByteOff !d !ptr = unsafeConsPeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafeConsPeekByteOff #-}
--
-- unsafeConsReadByteOff  :: CTX => Int -> Component comp layout -> m out
-- unsafeConsWriteByteOff :: CTX => Int -> Component comp layout ->   out -> m ()
-- unsafeConsReadByteOff  !d = unsafeConsPeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeConsReadByteOff  #-}
-- unsafeConsWriteByteOff !d = unsafeConsPokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeConsWriteByteOff #-}
--
-- readCons  :: CTX => ConsReader comp layer m => Component comp layout -> m out
-- writeCons :: CTX => ConsWriter comp layer m => Component comp layout -> out -> m ()
-- readCons  = readCons__  @comp @layer ; {-# INLINE readCons  #-}
-- writeCons = writeCons__ @comp @layer ; {-# INLINE writeCons #-}
--
-- #undef CTX
--
--
-- -- === Instances === --
--
-- instance {-# OVERLAPPABLE #-} Pass.LayerByteOffsetGetter comp layer m
--     => ConsReader comp layer m where
--     readCons__ !comp = do
--         !off <- Pass.getLayerByteOffset @comp @layer
--         unsafeConsReadByteOff @layer off comp
--     {-# INLINE readCons__ #-}
--
-- instance {-# OVERLAPPABLE #-} Pass.LayerByteOffsetGetter comp layer m
--     => ConsWriter comp layer m where
--     writeCons__ !comp !d = do
--         !off <- Pass.getLayerByteOffset @comp @layer
--         unsafeConsWriteByteOff @layer off comp d
--     {-# INLINE writeCons__ #-}
--
--
-- -- === Early resolution block === --
--
-- instance ConsReader Imp  layer m    where readCons__ _ = impossible
-- instance ConsReader comp Imp   m    where readCons__ _ = impossible
-- instance ConsReader comp layer ImpM where readCons__ _ = impossible
--
-- instance ConsWriter Imp  layer m    where writeCons__ _ _ = impossible
-- instance ConsWriter comp Imp   m    where writeCons__ _ _ = impossible
-- instance ConsWriter comp layer ImpM where writeCons__ _ _ = impossible
