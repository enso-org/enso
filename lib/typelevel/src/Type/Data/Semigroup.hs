{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE TypeInType   #-}

module Type.Data.Semigroup where

infixr 6 <>
type family (a :: k) <> (b :: k) :: k

