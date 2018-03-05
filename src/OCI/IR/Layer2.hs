{-# LANGUAGE CPP                  #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer2 where

import Prologue hiding (Data)
import Type.Data.Bool

import qualified OCI.IR.Layout     as Layout
import qualified OCI.Pass.Class    as Pass
import qualified Foreign.Storable  as Storable
import qualified Foreign.Storable1 as Storable1

import Foreign.Ptr            (plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable       (Storable)
import Foreign.Storable.Utils (sizeOf')
import Foreign.Storable1      (Storable1)
import OCI.IR.Component       (Component(Component))


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
type family ConsData   comp layer layout :: Cons
type family ConsLayout (cons :: Cons)    :: Type

type ConsData' comp layer layout = ConsData comp layer (Layout.GetBase layout) layout
-- type family View comp layer layout :: Type -> Type
--
-- type Data_       comp layer        = Data comp layer        ()
-- type View_       comp layer layout = View comp layer layout ()
-- type LayoutView' comp layer layout = View comp layer (Layout.GetBase layout)
-- type LayoutView_ comp layer layout = LayoutView' comp layer layout ()
-- type LayoutView  comp layer layout = LayoutView' comp layer layout layout -- (Layout.SubLayout layer layout)


type Layer     comp layer        = Storable1 (Data comp layer)
type LayerCons comp layer layout = Storable1 (ConsData comp layer (Layout.GetBase layout))


-- === General Information === --

byteSize :: ∀ comp layer. Layer comp layer => Int
byteSize = Storable1.sizeOf' @(Data comp layer) ; {-# INLINE byteSize #-}


-- === Layer.Data API === --

#define CTX ∀ layer comp layout m. (Layer comp layer, MonadIO m)

unsafePeek :: CTX => SomePtr -> m (Data comp layer layout)
unsafePoke :: CTX => SomePtr ->   (Data comp layer layout) -> m ()
unsafePeek !ptr = liftIO $ Storable1.peek (coerce ptr) ; {-# INLINE unsafePeek #-}
unsafePoke !ptr = liftIO . Storable1.poke (coerce ptr) ; {-# INLINE unsafePoke #-}

unsafePeekByteOff :: CTX => Int -> SomePtr -> m (Data comp layer layout)
unsafePokeByteOff :: CTX => Int -> SomePtr ->   (Data comp layer layout) -> m ()
unsafePeekByteOff !d !ptr = unsafePeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePeekByteOff #-}
unsafePokeByteOff !d !ptr = unsafePoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafePokeByteOff #-}

unsafeReadByteOff  :: CTX => Int -> Component comp layout -> m (Data comp layer layout)
unsafeWriteByteOff :: CTX => Int -> Component comp layout ->   (Data comp layer layout) -> m ()
unsafeReadByteOff  !d = unsafePeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeReadByteOff  #-}
unsafeWriteByteOff !d = unsafePokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeWriteByteOff #-}

#undef CTX


-- === Layer.ConsData API === --

#define CTX ∀ layer comp layout out cons baseLayout m. \
    ( out        ~ cons layout                         \
    , cons       ~ ConsData comp layer baseLayout      \
    , baseLayout ~ ConsLayout cons                     \
    , baseLayout ~ Layout.GetBase layout               \
    , LayerCons comp layer layout                      \
    , MonadIO m                                        \
    )

unsafeConsPeek :: CTX => SomePtr -> m out
unsafeConsPoke :: CTX => SomePtr ->   out -> m ()
unsafeConsPeek !ptr = liftIO $ Storable1.peekByteOff ptr constructorSize ; {-# INLINE unsafeConsPeek #-}
unsafeConsPoke !ptr = liftIO . Storable1.pokeByteOff ptr constructorSize ; {-# INLINE unsafeConsPoke #-}

unsafeConsPeekByteOff :: CTX => Int -> SomePtr -> m out
unsafeConsPokeByteOff :: CTX => Int -> SomePtr ->   out -> m ()
unsafeConsPokeByteOff !d !ptr = unsafeConsPoke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafeConsPokeByteOff #-}
unsafeConsPeekByteOff !d !ptr = unsafeConsPeek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE unsafeConsPeekByteOff #-}

unsafeConsReadByteOff  :: CTX => Int -> Component comp layout -> m out
unsafeConsWriteByteOff :: CTX => Int -> Component comp layout ->   out -> m ()
unsafeConsReadByteOff  !d = unsafeConsPeekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeConsReadByteOff  #-}
unsafeConsWriteByteOff !d = unsafeConsPokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeConsWriteByteOff #-}


class ConsReader comp layer m where
    readCons__  :: ∀ layout baseLayout cons out.
        ( out        ~ cons layout
        , cons       ~ ConsData comp layer baseLayout
        , baseLayout ~ ConsLayout cons
        , baseLayout ~ Layout.GetBase layout
        , Storable1 cons
        ) => Component comp layout -> m out

instance {-# OVERLAPPABLE #-}
    (MonadIO m, Pass.LayerByteOffsetGetter comp layer m)
    => ConsReader comp layer m where
    readCons__ comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeConsReadByteOff @layer off comp

#undef CTX



--------------------------------------
-- === Reader / Writer / Editor === --
--------------------------------------

-- === Definition === --

type Editor comp layer m =
   ( Reader comp layer m
   , Writer comp layer m
   )

class Reader comp layer m where
    read__  :: ∀ layout. Component comp layout -> m (Data comp layer layout)

class Writer comp layer m where
    write__ :: ∀ layout. Component comp layout -> Data comp layer layout -> m ()

read :: ∀ layer comp layout m. Reader comp layer m
     => Component comp layout -> m (Data comp layer layout)
read = read__ @comp @layer @m ; {-# INLINE read #-}

write :: ∀ layer comp layout m. Writer comp layer m
      => Component comp layout -> Data comp layer layout -> m ()
write = write__ @comp @layer @m ; {-# INLINE write #-}


-- === Implementation === --

instance {-# OVERLAPPABLE #-}
    (Layer comp layer, MonadIO m, Pass.LayerByteOffsetGetter comp layer m)
    => Reader comp layer m where
    read__ comp = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeReadByteOff @layer off comp
    {-# INLINE read__ #-}

instance {-# OVERLAPPABLE #-}
    (Layer comp layer, MonadIO m, Pass.LayerByteOffsetGetter comp layer m)
    => Writer comp layer m where
    write__ comp d = do
        !off <- Pass.getLayerByteOffset @comp @layer
        unsafeWriteByteOff @layer off comp d
    {-# INLINE write__ #-}


-- === Early resolution block === --

instance Reader Imp  layer m    where read__ _ = impossible
instance Reader comp Imp   m    where read__ _ = impossible
instance Reader comp layer ImpM where read__ _ = impossible

instance Writer Imp  layer m    where write__ _ _ = impossible
instance Writer comp Imp   m    where write__ _ _ = impossible
instance Writer comp layer ImpM where write__ _ _ = impossible
