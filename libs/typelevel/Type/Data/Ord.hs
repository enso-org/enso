
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE PolyKinds          #-}

module Type.Data.Ord where

import Prelude



type family (a :: k) > (b :: k) :: Bool
type family (a :: k) < (b :: k) :: Bool
