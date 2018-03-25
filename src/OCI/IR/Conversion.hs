{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module OCI.IR.Conversion where

import Prologue hiding (Castable, ConversionError)

import           Type.Error ((:<+>:))
import qualified Type.Error as Error


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


-- type family Relayout2 (src :: Type) (tgt :: Type) :: Bool
-- type family RelayoutOrEq src tgt where
--     RelayoutOrEq a a = 'True
--     RelayoutOrEq a b = Relayout2 a b

-- type relayout src tgt = Error.TypeErrorIf (Relayout2 src tgt)
--             (ConversionErrorMsg (Error.Str "Cannot relayoutxxx") src tgt)
-- relayout2 :: relayout src tgt => src -> tgt
-- relayout2 = unsafeCoerce ; {-# INLINE relayout2 #-}






-- FIXME[WD]: REMOVE VVV
-------------------
-- === Casts === --
-------------------

-- === Definition === --

-- class Coercible src tgt => Castable (src :: Type) (tgt :: Type)
-- instance {-# OVERLAPPABLE #-}
--     (Coercible src tgt, ConversionError' "Cannot cast" src tgt)
--     => Castable src tgt

-- cast :: Castable src tgt => src -> tgt
-- cast = coerce ; {-# INLINE cast #-}
