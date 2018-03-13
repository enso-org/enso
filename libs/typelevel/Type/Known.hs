{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeInType          #-}

module Type.Known (module Type.Known, module X) where

import GHC.TypeLits as X (KnownNat, KnownSymbol)

import Data.Convert
import Data.Kind    (Constraint, Type)
import Data.Proxy
import GHC.TypeLits
import Prelude


----------------------------------------
-- === Types to values conversion === --
----------------------------------------

-- === Definition === --

class Known t where from :: KnownVal t

type family KnownVal (t :: k) where
    KnownVal (t :: k) = KnownKindVal k

type family KnownKindVal (nat :: Type) where
    KnownKindVal Nat    = Integer
    KnownKindVal Symbol = String
    KnownKindVal t      = t

type family Knowns (ts :: [k]) :: Constraint where
    Knowns '[] = ()
    Knowns (t ': ts) = (Known t, Knowns ts)


-- === Utils === --

from' :: forall t s. (Known t, Convertible' (KnownVal t) s) => s
from' = convert' $ from @t ; {-# INLINE from' #-}


-- === Instances === --

instance KnownNat t => Known (t :: Nat) where
    from = natVal (Proxy @t) ; {-# INLINE from #-}

instance KnownSymbol t => Known (t :: Symbol) where
    from = symbolVal (Proxy @t) ; {-# INLINE from #-}

instance Known 'True  where from = True  ; {-# INLINE from #-}
instance Known 'False where from = False ; {-# INLINE from #-}

-- FIXME!
-- The infered constraint `KnownKindVal k ~ k`
-- is not correct. It fails for `foo = from @('Just 7)`
instance (Known a, KnownKindVal k ~ k)
      => Known ('Just (a :: k)) where from = Just $ from @a ; {-# INLINE from #-}
instance Known 'Nothing         where from = Nothing        ; {-# INLINE from #-}
