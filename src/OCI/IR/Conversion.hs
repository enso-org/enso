{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Conversion where

import Prologue hiding (ConversionError)

import qualified Type.Error as Error
import           Type.Error ((:<+>:))



------------------------------
-- === Type error utils === --
------------------------------

type family ConversionError' b s d where ConversionError' b s d = ConversionError (Error.Str b) s d
type family ConversionError  b s d where ConversionError  b s d = TypeError (ConversionErrorMsg b s d)
type ConversionErrorMsg (base :: ErrorMessage) (src :: Type) (dst :: Type)
    = Error.Sentence
     ( base
 :<+>: Error.Type src
 :<+>: Error.Str "to"
 :<+>: Error.Type dst
     )



----------------------------
-- === Generalization === --
----------------------------

-- === Definition === --

class Generalizable (src :: Type) (dst :: Type)
instance {-# OVERLAPPABLE #-} ConversionError' "Cannot generalize" src dst => Generalizable src dst

generalize :: Generalizable src dst => src -> dst
generalize = unsafeCoerce ; {-# INLINE generalize #-}



-------------------
-- === Casts === --
-------------------

-- === Definition === --

class UnsafeCastable (src :: Type) (dst :: Type)
instance {-# OVERLAPPABLE #-} ConversionError' "Cannot cast" src dst => UnsafeCastable src dst

unsafeCast :: UnsafeCastable src dst => src -> dst
unsafeCast = unsafeCoerce ; {-# INLINE unsafeCast #-}
