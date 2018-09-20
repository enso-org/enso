{-# LANGUAGE NoStrict     #-}
{-# LANGUAGE NoStrictData #-}
{-# LANGUAGE PolyKinds    #-}

module Type.Data.Maybe where

import GHC.TypeLits
import Prelude

type family IsJust a where
    IsJust ('Just a) = 'True
    IsJust a         = 'False

type family IsNothing a where
    IsNothing 'Nothing = 'True
    IsNothing a        = 'False

type family CatMaybes (lst :: [Maybe k]) :: [k] where
    CatMaybes '[]              = '[]
    CatMaybes ('Just a  ': ms) = a ': CatMaybes ms
    CatMaybes ('Nothing ': ms) =      CatMaybes ms

type family FromJust (m :: Maybe k) :: k where
    FromJust ('Just a) = a

type family FromMaybe a (m :: Maybe k) where
    FromMaybe _ ('Just a) = a
    FromMaybe a 'Nothing  = a

type family SuccMaybe (m :: Maybe Nat) where
    SuccMaybe ('Just n) = 'Just (n + 1)
    SuccMaybe 'Nothing  = 'Nothing
