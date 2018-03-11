{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component where

import Prologue hiding (Castable, ConversionError)

import qualified Data.Tag              as Tag
import qualified Foreign.Marshal.Utils as Mem
import qualified Foreign.Memory.Pool   as MemPool
import qualified Foreign.Ptr           as Ptr
import qualified Foreign.Storable      as Storable
import qualified Foreign.Storable1     as Storable1
import qualified Language.Haskell.TH   as TH
import qualified OCI.Pass.Class        as Pass
import qualified Type.Error            as Error

import Foreign.Ptr.Utils          (SomePtr)
import Foreign.Storable           (Storable)
import Foreign.Storable1.Deriving (deriveStorable1)
import OCI.IR.Conversion          (Castable, ConversionError, Generalizable)
import Type.Error                 ((:<+>:))


-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component     t (layout :: Type) = Component SomePtr deriving (Eq, Show, Storable)
type    SomeComponent t = Component t ()
makeLenses      ''Component
deriveStorable1 ''Component


-- === API === --

type Allocator comp m =
    ( MonadIO m
    , Pass.ComponentMemPoolGetter comp m
    )

type Creator comp m =
    ( Allocator                   comp m
    , Pass.LayerInitializerGetter comp m
    , Pass.ComponentSizeGetter    comp m
    )

unsafeNull :: Component comp layout
unsafeNull = Component Ptr.nullPtr ; {-# INLINE unsafeNull #-}

alloc :: ∀ comp m layout. Allocator comp m => m (Component comp layout)
alloc = do
    pool <- Pass.getComponentMemPool @comp
    wrap <$> MemPool.alloc pool
{-# INLINE alloc #-}

new :: ∀ comp m layout. Creator comp m => m (Component comp layout)
new = do
    ir   <- alloc
    ptr  <- Pass.getLayerInitializer @comp
    size <- Pass.getComponentSize    @comp
    liftIO $ Mem.copyBytes (coerce ir) ptr size
    pure ir
{-# INLINE new #-}



-- === Generalization === --

class GeneralizableComponent (t :: Type) (layout :: Type) (layout' :: Type)
instance GeneralizableComponent t layout ()

instance {-# OVERLAPPABLE #-}
    ConversionError (Error.Str "Cannot generalize" :<+>: 'Error.ShowType t) a b
 => GeneralizableComponent t a b

instance {-# OVERLAPPABLE #-}
    ( a ~ Component t layout'
    , GeneralizableComponent t layout layout'
    ) => Generalizable a (Component t layout)

instance {-# OVERLAPPABLE #-}
    ( a ~ Component t layout'
    , GeneralizableComponent t layout layout'
    ) => Generalizable (Component t layout) a

instance {-# OVERLAPPABLE #-}
    ( t ~ t'
    , GeneralizableComponent t layout layout'
    ) => Generalizable (Component t layout) (Component t' layout')


-- === Instances === --

instance {-# OVERLAPPABLE #-} a ~ Component t l' => Castable (Component t l) a
instance {-# OVERLAPPABLE #-} a ~ Component t l' => Castable a (Component t l)
instance t ~ t' => Castable (Component t l) (Component t' l')


-- === TH === --

define :: String -> TH.Q [TH.Dec]
define el = Tag.nonStandardFamilyInstance ''Component el (el <> "s")
