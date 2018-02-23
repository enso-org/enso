{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Component where

import Prologue hiding (ConversionError)

import Foreign.Storable  (Storable)
import Foreign.Ptr.Utils (SomePtr)
import OCI.IR.Conversion (Generalizable, ConversionError)
import Type.Error        ((:<+>:))

import qualified Type.Error as Error



-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component t cfg = Component SomePtr deriving (Eq, Show, Storable)
type SomeComponent t = Component t ()


-- === Generalization === --

class GeneralizableComponent (t :: Type) (cfg :: Type) (cfg' :: Type)
instance GeneralizableComponent t cfg ()

instance {-# OVERLAPPABLE #-} ConversionError (Error.Str "Cannot generalize" :<+>: Error.ShowType t) cfg cfg'
      => GeneralizableComponent t cfg cfg'

instance {-# OVERLAPPABLE #-} (a ~ Component t cfg', GeneralizableComponent t cfg cfg') => Generalizable a                 (Component t cfg)
instance {-# OVERLAPPABLE #-} (a ~ Component t cfg', GeneralizableComponent t cfg cfg') => Generalizable (Component t cfg) a
instance {-# OVERLAPPABLE #-} (t ~ t'              , GeneralizableComponent t cfg cfg') => Generalizable (Component t cfg) (Component t' cfg')
