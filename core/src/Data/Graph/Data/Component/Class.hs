{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Class
    (module Data.Graph.Data.Component.Class, module X) where
import Data.Construction as X (construct', construct1', destruct, destruct1)

import Prologue hiding (ConversionError)

import qualified Control.Monad.State.Layered      as State
import qualified Data.Construction                as Data
import qualified Data.Graph.Data.Layer.Class as Layer
import qualified Data.Tag                         as Tag
import qualified Foreign.Info.ByteSize            as ByteSize
import qualified Foreign.Marshal.Utils            as Mem
import qualified Foreign.Memory.Pool              as MemPool
import qualified Foreign.Ptr                      as Ptr
import qualified Foreign.Storable1.Deriving       as Storable1
import qualified Language.Haskell.TH              as TH

import Data.Graph.Data.Layer.Layout (Relayout, UnsafeRelayout)
import Foreign.Memory.Pool               (MemPool)
import Foreign.Ptr.Utils                 (SomePtr)
import Foreign.Storable                  (Storable)



-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component tag layout = Component SomePtr
    deriving (Eq, Ord, Show, Storable)
makeLenses       ''Component
Storable1.derive ''Component

type SomeComponent tag = Component tag ()


-- === Conversions === --

unsafeToPtr :: Component comp layout -> SomePtr
unsafeToPtr = coerce ; {-# INLINE unsafeToPtr #-}

unsafeFromPtr :: ∀ comp layout. SomePtr -> Component comp layout
unsafeFromPtr = coerce ; {-# INLINE unsafeFromPtr #-}

-- TODO: Change to UnsafeConvertible
instance Convertible (Component t a) SomePtr         where convert = coerce ; {-# INLINE convert #-}
instance Convertible SomePtr         (Component t a) where convert = coerce ; {-# INLINE convert #-}


-- === Construction / Destruction === --

type Allocator comp m =
    ( MonadIO m
    , State.Getter (MemPool (SomeComponent comp)) m
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


-- === Relayout === --

class Relayout__ tag layout layout'
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


-- === TH === --

define :: String -> TH.Q [TH.Dec]
define el = Tag.nonStandardFamilyInstance ''Component el (el <> "s")



-----------------
-- === Rep === --
-----------------

-- === Definition === --

newtype TagRep    = TagRep    SomeTypeRep deriving (Eq, Ord, Show)
newtype LayoutRep = LayoutRep SomeTypeRep deriving (Eq, Ord, Show)
data    Rep       = Rep TagRep LayoutRep  deriving (Eq, Ord, Show)
makeLenses ''TagRep
makeLenses ''LayoutRep
makeLenses ''Rep


-- === API === --

tagRep    :: ∀ tag        . Typeable tag             => TagRep
layoutRep :: ∀     layout . Typeable layout          => LayoutRep
rep       :: ∀ tag layout . Typeables '[tag, layout] => Rep
rep1      :: ∀ tag        . Typeable tag             => Rep
tagRep    = wrap $ someTypeRep @tag               ; {-# INLINE tagRep    #-}
layoutRep = wrap $ someTypeRep @layout            ; {-# INLINE layoutRep #-}
rep       = Rep (tagRep @tag) (layoutRep @layout) ; {-# INLINE rep       #-}
rep1      = rep @tag @()                          ; {-# INLINE rep1      #-}

repOf       :: ∀ t lyt. Typeables '[t, lyt] => Component t lyt -> Rep
tagRepOf    :: ∀ t lyt. Typeable  t         => Component t lyt -> TagRep
layoutRepOf :: ∀ t lyt. Typeable  lyt       => Component t lyt -> LayoutRep
repOf       _ = rep    @t @lyt ; {-# INLINE repOf       #-}
tagRepOf    _ = tagRep    @t   ; {-# INLINE tagRepOf    #-}
layoutRepOf _ = layoutRep @lyt ; {-# INLINE layoutRepOf #-}
