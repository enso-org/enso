{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component where

import Prologue hiding (ConversionError)

import Foreign.Storable  (Storable)
import Foreign.Ptr.Utils (SomePtr)
import OCI.IR.Conversion (Castable, Generalizable, ConversionError)
import Type.Error        ((:<+>:))

import qualified Data.Tag            as Tag
import qualified Language.Haskell.TH as TH
import qualified Type.Error          as Error



-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component t layout = Component SomePtr deriving (Eq, Show, Storable)
type SomeComponent t = Component t ()
makeLenses ''Component


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


-- === TH === --

componentInstance :: String -> TH.Q [TH.Dec]
componentInstance el = Tag.nonStandardFamilyInstance "Component" el (el <> "s")
