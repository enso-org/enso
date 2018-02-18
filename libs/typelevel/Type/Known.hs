{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType          #-}

module Type.Known (module Type.Known, module X) where

import GHC.TypeLits as X (KnownNat, KnownSymbol)

import Prelude
import Data.Convert
import Data.Kind (Type)
import Data.Proxy
import GHC.TypeLits


----------------------------------------
-- === Types to values conversion === --
----------------------------------------

-- === Definition === --

class KnownType t where fromType :: KnownTypeVal t

type family KnownTypeVal (t   :: k)    where KnownTypeVal (t :: k) = KnownKindVal k
type family KnownKindVal (nat :: Type) where KnownKindVal Nat      = Integer
                                             KnownKindVal Symbol   = String
                                             KnownKindVal t        = t


-- === Utils === --

fromType' :: forall t s. (KnownType t, Convertible' (KnownTypeVal t) s) => s
fromType' = convert' $ fromType @t ; {-# INLINE fromType' #-}


-- === Instances === --

instance KnownNat    t => KnownType (t :: Nat)    where fromType = natVal    (Proxy @t) ; {-# INLINE fromType #-}
instance KnownSymbol t => KnownType (t :: Symbol) where fromType = symbolVal (Proxy @t) ; {-# INLINE fromType #-}
