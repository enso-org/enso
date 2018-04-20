{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Class (module OCI.IR.Component.Class, module X) where
import Data.Construction as X (destruct, destruct1, new, new1)

import Prologue hiding (Castable, ConversionError)

import qualified Data.Construction          as Data
import qualified Data.Tag                   as Tag
import qualified Foreign.Marshal.Utils      as Mem
import qualified Foreign.Memory.Pool        as MemPool
import qualified Foreign.Ptr                as Ptr
import qualified Foreign.Storable           as Storable
import qualified Foreign.Storable1          as Storable1
import qualified Foreign.Storable1.Deriving as Storable1
import qualified Language.Haskell.TH        as TH
import qualified OCI.Pass.Definition        as Pass
import qualified Type.Error                 as Error

import Foreign.Ptr.Utils (SomePtr)
import Foreign.Storable  (Storable)
import OCI.IR.Layout     (Relayout, UnsafeRelayout)
import Type.Error        ((:<+>:))



-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component     t (layout :: Type) = Component SomePtr deriving (Eq, Show, Storable)
type    SomeComponent t = Component t ()
makeLenses       ''Component
Storable1.derive ''Component


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

unsafeNull :: Component comp layout
unsafeNull = Component Ptr.nullPtr ; {-# INLINE unsafeNull #-}

alloc :: ∀ comp m layout. Allocator comp m => m (Component comp layout)
alloc = do
    pool <- Pass.getComponentMemPool @comp
    wrap <$> MemPool.alloc pool
{-# INLINE alloc #-}

dispose :: Creator comp m => Component comp layout -> m ()
dispose = destruct ; {-# INLINE dispose #-}

instance Creator comp m => Data.Constructor1 () (Component comp) m where
    construct1 _ = do
        ir   <- alloc
        init <- Pass.getLayerMemManager @comp
        size <- Pass.getComponentSize   @comp
        let ptr = coerce ir
        liftIO $ Mem.copyBytes ptr (init ^. Pass.initializer) size
        liftIO $ (init ^. Pass.constructor) ptr
        pure ir
    {-# INLINE construct1 #-}

instance Creator comp m => Data.Destructor1 (Component comp) m where
    destruct1 ir = do
        init <- Pass.getLayerMemManager @comp
        liftIO $ (init ^. Pass.destructor) (coerce ir)
    {-# INLINE destruct1 #-}


-- === Relayout === --

class Relayout__ (t :: Type) (layout :: Type) (layout' :: Type)
instance {-# OVERLAPPABLE #-} Relayout l l'
      => Relayout__ t l l'
instance Relayout__ t l ()

instance {-# OVERLAPPABLE #-} (Relayout__ t l l', a ~ Component t l')
      => Relayout a (Component t l)

instance {-# OVERLAPPABLE #-} (Relayout__ t l l', a ~ Component t l')
      => Relayout (Component t l) a

instance {-# OVERLAPPABLE #-} (Relayout__ t l l', t ~ t')
      => Relayout (Component t l) (Component t' l')

instance {-# OVERLAPPABLE #-} a ~ Component t l'
      => UnsafeRelayout a (Component t l)

instance {-# OVERLAPPABLE #-} a ~ Component t l'
      => UnsafeRelayout (Component t l) a

instance {-# OVERLAPPABLE #-} t ~ t'
      => UnsafeRelayout (Component t l) (Component t' l')


-- === Conversions === --

-- TODO: Change to UnsafeConvertible
instance Convertible (Component t a) SomePtr         where convert = coerce ; {-# INLINE convert #-}
instance Convertible SomePtr         (Component t a) where convert = coerce ; {-# INLINE convert #-}


-- === TH === --

define :: String -> TH.Q [TH.Dec]
define el = Tag.nonStandardFamilyInstance ''Component el (el <> "s")


