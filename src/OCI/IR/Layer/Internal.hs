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

constructorSize :: Int
constructorSize = sizeOf' @Int ; {-# INLINE constructorSize #-}



-------------------
-- === Layer === --
-------------------

-- === Definition === --

type family Cons   comp layer        :: Type -> Type
type family Layout comp layer layout :: Type
type        Data   comp layer layout
          = Cons   comp layer (Layout comp layer layout)

type StorableData comp layer = Storable1       (Cons comp layer)
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

byteSize :: ∀ comp layer. StorableData comp layer => Int
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

#define CTX ∀ layer comp layout m. (StorableData comp layer, MonadIO m)

unsafePeek :: CTX => SomePtr -> m (Data comp layer layout)
unsafePoke :: CTX => SomePtr ->   (Data comp layer layout) -> m ()
unsafePeek !ptr = liftIO $ Storable1.peek (coerce ptr) ; {-# INLINE unsafePeek #-}
unsafePoke !ptr = liftIO . Storable1.poke (coerce ptr) ; {-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX => Int -> SomePtr -> m (Data comp layer layout)
unsafePokeByteOff :: CTX => Int -> SomePtr ->   (Data comp layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff ::
    CTX => Int -> Component comp layout -> m (Data comp layer layout)
unsafeReadByteOff  !d = unsafePeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeReadByteOff  #-}

unsafeWriteByteOff ::
    CTX => Int -> Component comp layout -> (Data comp layer layout) -> m ()
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
    ( StorableData comp layer
    , Pass.LayerByteOffsetGetter comp layer (Pass pass)
    ) => Reader comp layer (Pass pass) where
    read__ !comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    ( StorableData comp layer
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
--    ( ConsGetter comp layer m
--    , ConsSetter comp layer m
--    )
--
-- class ConsGetter comp layer m where
--     getCons__ :: ∀ layout baseLayout cons out. CTXBody
--                => Component comp layout -> m out
--
-- class ConsSetter comp layer m where
--     setCons__ :: ∀ layout baseLayout cons out. CTXBody
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
-- getCons  :: CTX => ConsGetter comp layer m => Component comp layout -> m out
-- setCons :: CTX => ConsSetter comp layer m => Component comp layout -> out -> m ()
-- getCons  = getCons__  @comp @layer ; {-# INLINE getCons  #-}
-- setCons = setCons__ @comp @layer ; {-# INLINE setCons #-}
--
-- #undef CTX
--
--
-- -- === Instances === --
--
-- instance {-# OVERLAPPABLE #-} Pass.LayerByteOffsetGetter comp layer m
--     => ConsGetter comp layer m where
--     getCons__ !comp = do
--         !off <- Pass.getLayerByteOffset @comp @layer
--         unsafeConsReadByteOff @layer off comp
--     {-# INLINE getCons__ #-}
--
-- instance {-# OVERLAPPABLE #-} Pass.LayerByteOffsetGetter comp layer m
--     => ConsSetter comp layer m where
--     setCons__ !comp !d = do
--         !off <- Pass.getLayerByteOffset @comp @layer
--         unsafeConsWriteByteOff @layer off comp d
--     {-# INLINE setCons__ #-}
--
--
-- -- === Early resolution block === --
--
-- instance ConsGetter Imp  layer m    where getCons__ _ = impossible
-- instance ConsGetter comp Imp   m    where getCons__ _ = impossible
-- instance ConsGetter comp layer ImpM where getCons__ _ = impossible
--
-- instance ConsSetter Imp  layer m    where setCons__ _ _ = impossible
-- instance ConsSetter comp Imp   m    where setCons__ _ _ = impossible
-- instance ConsSetter comp layer ImpM where setCons__ _ _ = impossible
