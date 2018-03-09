{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component where

import Prologue hiding (Castable, ConversionError)

import qualified Data.Tag            as Tag
import qualified Foreign.Ptr         as Ptr
import qualified Foreign.Storable    as Storable
import qualified Foreign.Storable1   as Storable1
import qualified Language.Haskell.TH as TH
import qualified Type.Error          as Error

import Foreign.Ptr.Utils (SomePtr)
import Foreign.Storable  (Storable)
import OCI.IR.Conversion (Castable, ConversionError, Generalizable)
import Type.Error        ((:<+>:))



-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component t layout = Component SomePtr deriving (Eq, Show, Storable)
type SomeComponent t = Component t ()
makeLenses ''Component


-- === API === --

unsafeNull :: Component comp layout
unsafeNull = Component Ptr.nullPtr ; {-# INLINE unsafeNull #-}


-- === Generalization === --

class GeneralizableComponent (t :: Type) (layout :: Type) (layout' :: Type)
instance GeneralizableComponent t layout ()

instance {-# OVERLAPPABLE #-}
    ConversionError (Error.Str "Cannot generalize" :<+>: Error.ShowType t) a b
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

instance Storable1.Storable1 (Component t) where
    sizeOf    = Storable.sizeOf    ; {-# INLINE sizeOf    #-}
    alignment = Storable.alignment ; {-# INLINE alignment #-}
    peek      = Storable.peek      ; {-# INLINE peek      #-}
    poke      = Storable.poke      ; {-# INLINE poke      #-}



-- === TH === --

componentInstance :: String -> TH.Q [TH.Dec]
componentInstance el = Tag.nonStandardFamilyInstance "Component" el (el <> "s")
