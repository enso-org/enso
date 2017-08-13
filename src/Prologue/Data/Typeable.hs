{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prologue.Data.Typeable (module Prologue.Data.Typeable, module X) where

import Prelude
import Data.Kind
import Control.Lens
import Control.Lens.Utils
import Data.Convert
import qualified Data.Typeable as T
import           Data.Typeable as X (Typeable, Proxy(Proxy), typeOf, TypeRep)


---------------------
-- === TypeRep === --
---------------------

-- === Conversions === --

type FromTypeRep a = Convertible'   TypeRep a
type ToTypeRep   a = Convertible'   a TypeRep
type IsTypeRep   a = BiConvertible' TypeRep a

asTypeRep :: IsTypeRep a => Iso' a TypeRep
asTypeRep = converted' ; {-# INLINE asTypeRep #-}

switchedTypeRep :: (IsTypeRep a, IsTypeRep b) => Lens' a b
switchTypeRep   :: (IsTypeRep a, IsTypeRep b) => a -> b
switchedTypeRep = asTypeRep . from asTypeRep ; {-# INLINE switchedTypeRep #-}
switchTypeRep   = view switchedTypeRep       ; {-# INLINE switchTypeRep   #-}

instance Convertible TypeRep String where convert = show ; {-# INLINE convert #-}


----------------------
-- === Typeable === --
----------------------

typeOfProxy :: forall proxy a. Typeable a => proxy a -> TypeRep
typeOfProxy = T.typeRep ; {-# INLINE typeOfProxy #-}

typeRep  :: forall a           t. (Typeable  a, FromTypeRep t)  => t
typeRep' :: forall a           t.  Typeable  a                  => TypeRep
typeReps :: forall (ls :: [*]) t. (Typeables ls, FromTypeRep t) => [t]
typeRep  = convert' $ typeRep' @a         ; {-# INLINE typeRep  #-}
typeRep' = typeOfProxy (Proxy :: Proxy a) ; {-# INLINE typeRep' #-}
typeReps = convert' <$> typeReps' @ls     ; {-# INLINE typeReps #-}

class Typeables (ls :: [*]) where typeReps' :: [TypeRep]
instance (Typeable l, Typeables ls)
      => Typeables (l ': ls)   where typeReps' = typeRep @l : typeReps @ls ; {-# INLINE typeReps' #-}
instance Typeables '[]         where typeReps' = []                        ; {-# INLINE typeReps' #-}
