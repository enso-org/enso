{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE NoStrict             #-}
{-# LANGUAGE NoStrictData         #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Type.Known (module Type.Known, module X) where

import GHC.TypeLits as X (KnownNat, KnownSymbol)

import Prelude

import Data.Convert (Convertible', convert')
import Data.Kind    (Constraint, Type)
import Data.Proxy   (Proxy (Proxy))
import GHC.TypeLits (type (-), Nat, Symbol, natVal, symbolVal)



--------------------------------------
-- === Type -> Value conversion === --
--------------------------------------

-- === Definition === --

class Known  t   where val  :: KnownVal t
class Known' t v where val' :: v


-- === Internal === --

instance {-# OVERLAPPABLE #-} (Known t, Convertible' (KnownVal t) v)
      => Known' t v where
    val' = convert' (val @t :: KnownVal t)
    {-# INLINE val' #-}

type family KnownVal (t :: k) where
    KnownVal (t :: k) = KnownKindVal k

type family KnownKindVal (nat :: Type) where
    KnownKindVal Nat    = Integer
    KnownKindVal Symbol = String
    KnownKindVal t      = t

type family Knowns (ts :: [k]) :: Constraint where
    Knowns '[] = ()
    Knowns (t ': ts) = (Known t, Knowns ts)



-----------------------
-- === Instances === --
-----------------------

-- === Nat === --

instance KnownNat t
      => Known (t :: Nat) where
    val = natVal (Proxy @t)
    {-# INLINE val #-}

type KnownInt n = Known' n Int
instance {-# OVERLAPPABLE #-} KnownInt (n - 1)
      => Known' (n :: Nat) Int where
    val' = 1 + val' @(n - 1)
    {-# INLINE val' #-}

instance Known' (0 :: Nat) Int where
    val' = 0
    {-# INLINE val' #-}


-- === Symbol === --

instance KnownSymbol t
      => Known (t :: Symbol) where
    val = symbolVal (Proxy @t)
    {-# INLINE val #-}


-- === Bool === --

instance Known 'True  where val = True  ; {-# INLINE val #-}
instance Known 'False where val = False ; {-# INLINE val #-}



-- FIXME!
-- The infered constraint `KnownKindVal k ~ k`
-- is not correct. It fails for `foo = val @('Just 7)`
instance (Known a, KnownKindVal k ~ k)
      => Known ('Just (a :: k)) where val = Just $ val @a ; {-# INLINE val #-}
instance Known 'Nothing         where val = Nothing       ; {-# INLINE val #-}
