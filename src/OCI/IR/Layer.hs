{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Layer where

import Prologue hiding (Data)
import Type.Data.Bool

import qualified OCI.IR.Layout    as Layout
import qualified OCI.Pass.Class   as Pass
import qualified Foreign.Storable as Storable

import Foreign.Ptr            (plusPtr)
import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable       (Storable)
import Foreign.Storable.Utils (sizeOf')
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

type family Data comp layer        :: Type -> Type
type family View comp layer layout :: Type -> Type

type Data_       comp layer        = Data comp layer        ()
type View_       comp layer layout = View comp layer layout ()
type LayoutView' comp layer layout = View comp layer (Layout.GetBase layout)
type LayoutView_ comp layer layout = LayoutView' comp layer layout ()
type LayoutView  comp layer layout = LayoutView' comp layer layout layout -- (Layout.SubLayout layer layout)



-- === Storable === --

type StorableLayer comp layer layout
   = StorableView comp layer (Layout.GetBase layout)

class Storable (View_ comp layer layout)
   => StorableView comp layer layout where
    peekViewIO :: SomePtr -> IO (View_ comp layer layout)
    pokeViewIO :: SomePtr -> View_ comp layer layout -> IO ()

    default peekViewIO :: AutoStorableView comp layer layout
               => SomePtr -> IO (View_ comp layer layout)
    default pokeViewIO :: AutoStorableView comp layer layout
               => SomePtr -> View_ comp layer layout -> IO ()
    peekViewIO = autoPeekViewIO @comp @layer @layout; {-# INLINE peekViewIO #-}
    pokeViewIO = autoPokeViewIO @comp @layer @layout; {-# INLINE pokeViewIO #-}

instance {-# OVERLAPPABLE #-}
    ( Storable (View_ comp layer layout)
    , AutoStorableView comp layer layout
    ) => StorableView comp layer layout



-- === Automatic storable resolution === --

-- | AutoStorableView discovers wheter
--   'Layer comp layer' and 'View comp layer layout' are the same struct.
--   If not, then View's peek and poke implementation will skip the
--   constructor field. If that behavior is not intended, please provide
--   custom 'StorableView' implementation instead.

type AutoStorableView comp layer layout = AutoStorableView__
     (IsLayerFullView comp layer layout) comp layer layout

type IsLayerFullView comp layer layout
   = (Data comp layer != View comp layer layout)

class AutoStorableView__ (skipCons :: Bool) comp layer layout where
    autoPeekViewIO__ :: SomePtr -> IO (View_ comp layer layout)
    autoPokeViewIO__ :: SomePtr -> View_ comp layer layout -> IO ()

instance Storable (View_ comp layer layout)
      => AutoStorableView__ 'True comp layer layout where
    autoPeekViewIO__ !ptr = Storable.peek (ptr `plusPtr` constructorSize) ; {-# INLINE autoPeekViewIO__ #-}
    autoPokeViewIO__ !ptr = Storable.poke (ptr `plusPtr` constructorSize) ; {-# INLINE autoPokeViewIO__ #-}

instance Storable (View_ comp layer layout)
      => AutoStorableView__ 'False comp layer layout where
    autoPeekViewIO__ !ptr = Storable.peek (coerce ptr) ; {-# INLINE autoPeekViewIO__ #-}
    autoPokeViewIO__ !ptr = Storable.poke (coerce ptr) ; {-# INLINE autoPokeViewIO__ #-}

autoPeekViewIO :: ∀ c l s. AutoStorableView c l s
                    => SomePtr -> IO (View_ c l s)
autoPeekViewIO = autoPeekViewIO__ @(IsLayerFullView c l s) @c @l @s ; {-# INLINE autoPeekViewIO #-}

autoPokeViewIO :: ∀ c l s. AutoStorableView c l s
                    => SomePtr -> View_ c l s -> IO ()
autoPokeViewIO = autoPokeViewIO__ @(IsLayerFullView c l s) @c @l @s ; {-# INLINE autoPokeViewIO #-}



-- === API === --

type Layer comp layer = Storable (Data_ comp layer)

byteSize :: ∀ comp layer. Layer comp layer => Int
byteSize = Storable.sizeOf (undefined :: Data_ comp layer) ; {-# INLINE byteSize #-}


#define CTX ∀ layer comp layout m. (StorableLayer comp layer layout, MonadIO m)

peekSome :: CTX => SomePtr -> m (LayoutView_ comp layer layout)
pokeSome :: CTX => SomePtr -> (LayoutView_ comp layer layout) -> m ()
peekSome = liftIO .  peekViewIO @comp @layer @(Layout.GetBase layout) ; {-# INLINE peekSome #-}
pokeSome = liftIO .: pokeViewIO @comp @layer @(Layout.GetBase layout) ; {-# INLINE pokeSome #-}

peek :: CTX => SomePtr -> m (LayoutView comp layer layout)
poke :: CTX => SomePtr ->   (LayoutView comp layer layout) -> m ()
peek !ptr    = fmap unsafeCoerce $ peekSome @layer @comp @layout ptr ; {-# INLINE peek #-}
poke !ptr !v = pokeSome @layer @comp @layout ptr (unsafeCoerce v)    ; {-# INLINE poke #-}

peekByteOff :: CTX => Int -> SomePtr -> m (LayoutView comp layer layout)
pokeByteOff :: CTX => Int -> SomePtr ->   (LayoutView comp layer layout) -> m ()
peekByteOff !d !ptr = peek @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE peekByteOff #-}
pokeByteOff !d !ptr = poke @layer @comp @layout (ptr `plusPtr` d) ; {-# INLINE pokeByteOff #-}

unsafeReadByteOff  :: CTX => Int -> Component comp layout -> m (LayoutView comp layer layout)
unsafeWriteByteOff :: CTX => Int -> Component comp layout ->   (LayoutView comp layer layout) -> m ()
unsafeReadByteOff  !d = peekByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeReadByteOff  #-}
unsafeWriteByteOff !d = pokeByteOff @layer @comp @layout d . coerce ; {-# INLINE unsafeWriteByteOff #-}


read :: CTX => Pass.PassDataGetter (Pass.LayerByteOffset comp layer) m
     => Component comp layout -> m (LayoutView comp layer layout)
read comp = do
    Pass.LayerByteOffset !off <- Pass.getPassData @(Pass.LayerByteOffset comp layer)
    unsafeReadByteOff @layer off comp
{-# INLINE read #-}

#undef CTX
