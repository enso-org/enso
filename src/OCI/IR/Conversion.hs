{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Conversion where

import Prologue hiding (Castable, ConversionError)

import qualified Type.Error as Error
import           Type.Error ((:<+>:))



------------------------------
-- === Type error utils === --
------------------------------

type family ConversionError' b s d where
            ConversionError' b s d = ConversionError (Error.Str b) s d
type family ConversionError  b s d where
            ConversionError  b s d = TypeError (ConversionErrorMsg b s d)

type ConversionErrorMsg (base :: ErrorMessage) (src :: Type) (tgt :: Type)
    = Error.Sentence ( base :<+>: Error.Type src
                            :<+>: Error.Str "to"
                            :<+>: Error.Type tgt )



----------------------------
-- === Generalization === --
----------------------------

-- === Definition === --

class Coercible src tgt => Generalizable (src :: Type) (tgt :: Type)
instance {-# OVERLAPPABLE #-}
    (Coercible src tgt, ConversionError' "Cannot generalize" src tgt)
    => Generalizable src tgt

generalize :: Generalizable src tgt => src -> tgt
generalize = coerce ; {-# INLINE generalize #-}



-------------------
-- === Casts === --
-------------------

-- === Definition === --

class Coercible src tgt => Castable (src :: Type) (tgt :: Type)
instance {-# OVERLAPPABLE #-}
    (Coercible src tgt, ConversionError' "Cannot cast" src tgt)
    => Castable src tgt

cast :: Castable src tgt => src -> tgt
cast = coerce ; {-# INLINE cast #-}
