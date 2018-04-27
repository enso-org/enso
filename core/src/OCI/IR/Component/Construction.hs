{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Construction
    (module OCI.IR.Component.Construction, module X) where
import Data.Construction as X (destruct, destruct1, new, new1)

import Prologue hiding (ConversionError)

import qualified Data.Construction          as Data
import qualified Data.Tag                   as Tag
import qualified Foreign.Marshal.Utils      as Mem
import qualified Foreign.Memory.Pool        as MemPool
import qualified Foreign.Ptr                as Ptr
import qualified Foreign.Storable1.Deriving as Storable1
import qualified Language.Haskell.TH        as TH
import qualified OCI.IR.Component.Class     as Component
import qualified OCI.Pass.Definition        as Pass

import Foreign.Ptr.Utils      (SomePtr)
import Foreign.Storable       (Storable)
import OCI.IR.Component.Class (Component (Component))
import OCI.IR.Layout          (Relayout, UnsafeRelayout)



------------------------------------
-- === Component construction === --
------------------------------------

-- === API === --

type Allocator comp m =
    ( MonadIO m
    , Pass.ComponentMemPoolGetter comp m
    )

type Creator comp m =
    ( Allocator                  comp m
    , Pass.LayerMemManagerGetter comp m
    , Pass.ComponentSizeGetter   comp m
    )


type HasSize comp m = Pass.ComponentSizeGetter comp m

-- type HasPointers comp m =
--     ( MonadIO m
--     , Pass.DynamicGetterGetter comp m
--     )

unsafeNull :: Component comp layout
unsafeNull = Component Ptr.nullPtr ; {-# INLINE unsafeNull #-}

alloc :: ∀ comp m layout. Allocator comp m => m (Component comp layout)
alloc = do
    pool <- Pass.getComponentMemPool @comp
    wrap <$> MemPool.alloc pool
{-# INLINE alloc #-}

dealloc :: ∀ comp m layout. Allocator comp m => Component comp layout -> m ()
dealloc comp = do
    pool <- Pass.getComponentMemPool @comp
    MemPool.free pool $ unwrap comp
{-# INLINE dealloc #-}


-- === Construction / Destruction === --

instance Creator comp m => Data.Constructor1 m () (Component comp) where
    construct1 _ = do
        ir    <- alloc
        layer <- Pass.getLayerMemManager @comp
        size  <- Pass.getComponentSize   @comp
        let ptr = coerce ir
        liftIO $ Mem.copyBytes ptr (layer ^. Pass.initializer) size
        liftIO $ (layer ^. Pass.constructor) ptr
        pure ir
    {-# INLINE construct1 #-}

instance Creator comp m => Data.Destructor1 m (Component comp) where
    destruct1 ir = do
        layer <- Pass.getLayerMemManager @comp
        liftIO $ (layer ^. Pass.destructor) (coerce ir)
        dealloc ir
    {-# INLINE destruct1 #-}

instance Monad m => Data.ShallowDestructor1 m (Component comp) where
    destructShallow1 = const $ pure () ; {-# INLINE destructShallow1 #-}

byteSize :: ∀ comp m l. HasSize comp m => Component comp l -> m Int
byteSize _ = Pass.getComponentSize @comp

-- pointers :: ∀ comp m l. HasPointers comp m
--          => Component comp l -> m [SomePtr]
-- pointers comp = do
--     let ptr      = Component.unsafeToPtr comp
--     getPointers <- unwrap <$> Pass.getDynamicGetter @comp
--     liftIO $ getPointers ptr
