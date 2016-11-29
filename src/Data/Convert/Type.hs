{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}

-- | This module provides utils for type-level conversions
module Data.Convert.Type where

import Prelude


type family To (t :: *) (a :: k) :: k
