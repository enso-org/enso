{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component.Class (module OCI.IR.Component.Class, module X) where
import Data.Construction as X (destruct, destruct1, new, new1)

import Prologue hiding (ConversionError)

import qualified Data.Construction          as Data
import qualified Data.Tag                   as Tag
import qualified Foreign.Marshal.Utils      as Mem
import qualified Foreign.Memory.Pool        as MemPool
import qualified Foreign.Ptr                as Ptr
import qualified Foreign.Storable1.Deriving as Storable1
import qualified Language.Haskell.TH        as TH

import Foreign.Ptr.Utils (SomePtr)
import Foreign.Storable  (Storable)
import OCI.IR.Layout     (Relayout, UnsafeRelayout)



-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component (t :: Type) (layout :: Type) = Component SomePtr
    deriving (Eq, Ord, Show, Storable)
makeLenses       ''Component
Storable1.derive ''Component


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

unsafeToPtr :: Component comp layout -> SomePtr
unsafeToPtr = coerce ; {-# INLINE unsafeToPtr #-}

unsafeFromPtr :: ∀ comp layout. SomePtr -> Component comp layout
unsafeFromPtr = coerce ; {-# INLINE unsafeFromPtr #-}

-- TODO: Change to UnsafeConvertible
instance Convertible (Component t a) SomePtr         where convert = coerce ; {-# INLINE convert #-}
instance Convertible SomePtr         (Component t a) where convert = coerce ; {-# INLINE convert #-}


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
