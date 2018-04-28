{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Construction
    (module OCI.IR.Component.Construction, module X) where
import Data.Construction as X (destruct, destruct1, new, new1)

import Prologue hiding (ConversionError)

import qualified Control.Monad.State.Layered as State
import qualified Data.Construction           as Data
import qualified Data.Graph.Component.Class  as Component
import qualified Data.Tag                    as Tag
import qualified Foreign.Info.ByteSize       as ByteSize
import qualified Foreign.Marshal.Utils       as Mem
import qualified Foreign.Memory.Pool         as MemPool
import qualified Foreign.Ptr                 as Ptr
import qualified Foreign.Storable1.Deriving  as Storable1
import qualified Language.Haskell.TH         as TH
import qualified OCI.IR.Layer                as Layer

import Data.Graph.Component.Class (Component (Component))
import Foreign.Memory.Pool        (MemPool)
import Foreign.Ptr.Utils          (SomePtr)
import Foreign.Storable           (Storable)
import OCI.IR.Layout              (Relayout, UnsafeRelayout)



------------------------------------
-- === Component construction === --
------------------------------------

-- === API === --

type Allocator comp m =
    ( MonadIO m
    , State.Getter (MemPool (Component comp ())) m
    )

type Creator comp m =
    ( Allocator comp m
    , State.Getter (Layer.DynamicManager comp) m
    , ByteSize.Known (Component comp) m
    )


unsafeNull :: Component comp layout
unsafeNull = Component Ptr.nullPtr ; {-# INLINE unsafeNull #-}

alloc :: ∀ comp m layout. Allocator comp m => m (Component comp layout)
alloc = do
    pool <- State.get @(MemPool (Component comp ()))
    wrap <$> MemPool.alloc pool
{-# INLINE alloc #-}

dealloc :: ∀ comp m layout. Allocator comp m => Component comp layout -> m ()
dealloc comp = do
    pool <- State.get @(MemPool (Component comp ()))
    MemPool.free pool $ unwrap comp
{-# INLINE dealloc #-}


-- === Construction / Destruction === --

instance Creator comp m => Data.Constructor1 m () (Component comp) where
    construct1 _ = do
        ir    <- alloc
        layer <- State.get @(Layer.DynamicManager comp)
        size  <- ByteSize.get @(Component comp)
        let ptr = coerce ir
        liftIO $ Mem.copyBytes ptr (layer ^. Layer.dynamicInitializer) size
        liftIO $ (layer ^. Layer.dynamicConstructor) ptr
        pure ir
    {-# INLINE construct1 #-}

instance Creator comp m => Data.Destructor1 m (Component comp) where
    destruct1 ir = do
        layer <- State.get @(Layer.DynamicManager comp)
        liftIO $ (layer ^. Layer.dynamicDestructor) (coerce ir)
        dealloc ir
    {-# INLINE destruct1 #-}

instance Monad m => Data.ShallowDestructor1 m (Component comp) where
    destructShallow1 = const $ pure () ; {-# INLINE destructShallow1 #-}




-- type HasSize comp m = Pass.ComponentSizeGetter comp m

-- type HasPointers comp m =
--     ( MonadIO m
--     , Pass.DynamicGetterGetter comp m
--     )


-- byteSize :: ∀ comp m l. HasSize comp m => Component comp l -> m Int
-- byteSize _ = Pass.getComponentSize @comp

-- pointers :: ∀ comp m l. HasPointers comp m
--          => Component comp l -> m [SomePtr]
-- pointers comp = do
--     let ptr      = Component.unsafeToPtr comp
--     getPointers <- unwrap <$> Pass.getDynamicGetter @comp
--     liftIO $ getPointers ptr
